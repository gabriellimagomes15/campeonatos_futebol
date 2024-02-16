"====> Projeto análise futebol(football)
- https://www.football-data.org/pricing
- https://github.com/openfootball    
- https://openfootball.github.io/
  - https://datahub.io/collections/football
- https://www.api-football.com/pricing
- https://www.football-data.co.uk/brazil.php (partidas)
- https://www.espn.com.br/futebol/times/_/liga/ENG.1/premier-league (elenco, classificação etc)
- https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2012
"

source('utils.R')
source('charts.R')

#ano <- 2014
#liga <- lista_ligas[1]
#url

#### >>> SCRAPING FUTEBOL <<< ####
#### TABELA SITE ESPN ####

lista_ligas  <- c('ita.1','ENG.1','bra.1','esp.1')
for (liga in lista_ligas) {
  tabela_final <- data.frame()
  print(liga)
  for (ano in 2006:2011){
    y <- runif(n = 1,min = 20,max = 40)
    cat(ano,' - ',y,'\n')
    Sys.sleep(y)
    url          <- paste("https://www.espn.com.br/futebol/classificacao/_/liga/",liga,'/temporada/',ano,sep='')
    pagina <- read_html(url)
    
    tabela_times <- pagina %>% html_nodes('table .Table__even .Table__TD .team-link ') # %>% html_table() #
    times <- data.frame(posicao = tabela_times %>% html_nodes('.team-position') %>% html_text(),
                        time = tabela_times %>% html_nodes('.hide-mobile') %>% html_text())
    
    tabela_pontos <- pagina %>% html_nodes('table') %>% html_table() #[[1]][[1]]
    tabela <- cbind(ano,times,as.data.frame(time = lista_times,tabela_pontos[[2]]))
    
    tabela_final <- rbind(tabela_final, tabela)
    write.csv(tabela_final,paste('tabela_',liga,'2.csv',sep = ''),row.names = F)
    
    Sys.sleep(runif(n = 1,min = 20,max = 40))
  }
}


#### TABELA SITE CBF ####

tabela_final <- data.frame()
ano <- 2018

for (ano in 2006:2019){
  #cat(ano,'-')
  y <- runif(n = 1,min = 20,max = 40)
  cat(ano,' - ',y,'\n')
  Sys.sleep(y)
  
  url    <- paste0("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/",ano)
  pagina <- read_html(url)
  tabela <- pagina %>% html_node('.tabela-expandir') %>% html_table()
  tabela <- tabela[grep('^\\d+º',tabela$Posição),]
  
  split_ <- sapply(tabela$Posição,function(x) {
    #x <- tabela$Posição[16]
    r <- strsplit(x = x,split = '\n')[[1]]
    r <- r[grepl('^\\d+º|(\\-)(\\W+\\w+)',x = r)]
    split_ <- sapply(r, function(x) trimws(gsub(pattern = '\\º|\\s+',' ',x)) )
  })
  #split_
  
  df_tabela <- data.frame( t(data.frame(split_)),row.names = NULL)
  colnames(df_tabela) <- c('posicao','time')
  df_tabela$ano <- ano

  tabela_final <- rbind(cbind(df_tabela,tabela %>% select(-Posição)),
                        tabela_final)
  write.csv(tabela_final,'tabela_bra_cbf.csv',row.names = F)
}


#### JOGOS (PLACAR, CARTOES etc) ####

# 'https://www.football-data.co.uk/mmz4281/0506/E0.csv' - ENG
# 'https://www.football-data.co.uk/mmz4281/0506/I1.csv' - ITA
# 'https://www.football-data.co.uk/mmz4281/0506/SP1.csv' - ESP


lista_ligas  <- c('E0','I1','SP1')
for (liga in lista_ligas) {
  #liga <- lista_ligas[1]
  tabela_final <- data.frame()
  print(liga)
  for (ano in 6:19){
    #ano <- 7
    y <- runif(n = 1,min = 20,max = 40)
    cat(ano,' - ',y,'\n')
    Sys.sleep(y)
    ano_inic <- formatC(ano, width = 2, format = "d", flag = "0")
    ano_fim  <- formatC(ano+1, width = 2, format = "d", flag = "0")
    url      <- paste0("https://www.football-data.co.uk/mmz4281/",ano_inic,ano_fim,'/',liga,".csv")
    df_jogos <- fread(url)
    
    tabela_final <- rbind(tabela_final,df_jogos,fill = T)
    write.csv(tabela_final,paste0('jogos_',liga,'.csv'),row.names = F)
  }
} 
bra <- fread('https://www.football-data.co.uk/new/BRA.csv')    
write.csv(bra,'jogos_bra.csv',row.names = F)

    
#### >>> TRATAMENTO DOS DADOS <<< ####

#### TRATAMENTO TABELAS ####
ita <- fread('tabela_ita.1.csv')
ita <- rbind(ita,fread('tabela_ita.12.csv'))
ita$pais <- 'ITA'

esp <- fread('tabela_esp.1.csv')
esp <- rbind(esp,fread('tabela_esp.12.csv'),fill = T)
esp$pais <- 'ESP'

eng <- fread('tabela_ENG.1.csv')
eng <- rbind(eng,fread('tabela_ENG.12.csv'), fill = T)
eng$pais <- 'ENG'

bra <- fread('tabela_bra.1.csv')
bra <- rbind(bra,fread('tabela_bra.12.csv'), fill = T)
bra$pais <- 'BRA'
bra$time <- gsub(pattern = 'América \\(MG\\)',replacement = 'América-MG',bra$time,perl = T)


tabela_campeonatos <- rbind(ita,esp,eng,bra)
tabela_campeonatos <- tabela_campeonatos %>% select(-V1)
tabela_campeonatos <- distinct(.keep_all = T)
write.csv(tabela_campeonatos,'tabela_campeonatos.csv',row.names = F)

#### TRATAMENTO JOGOS ####
jogos_eng <- fread('jogos_E0.csv')
jogos_eng$pais <- 'ENG'

jogos_ita <- fread('jogos_I1.csv')
jogos_ita$pais <- 'ITA'

jogos_esp <- fread('jogos_SP1.csv')
jogos_esp$pais <- 'ESP'

jogos_bra <- fread('jogos_bra.csv')
jogos_bra$pais <- 'BRA'
jogos_bra <- jogos_bra %>% select(Date,Home,Away,HG,AG,Res,pais)
colnames(jogos_bra) <- c('Date','HomeTeam','AwayTeam','FTHG','FTAG','FTR','pais')

jogos_campeonatos <- rbind(jogos_eng,jogos_ita,jogos_esp,jogos_bra, fill = T)
jogos_campeonatos <- jogos_campeonatos %>% select(Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR,
                                                  HY,HR,AY,AR,pais)

colnames(jogos_campeonatos) <- c('data','time_casa','time_visit','gols_casa','gols_visit',
                                 'resultado','cartao_amarelo_casa','cartao_vermelho_casa',
                                 'cartao_amarelo_visit','cartao_vermelho_visit','pais')

write.csv(jogos_campeonatos,'jogos_campeonatos.csv',row.names = F)


#### TRATAMENTO NOME TIMES ####

lista_ligas <- c('BRA','ITA','ESP','ENG')
lista_times <- fread('lista_times_clean.csv')

tabela <- fread('tabela_campeonatos.csv')
tabela_limpos <- data.frame()
for (pais_selec in lista_ligas) {
  #pais_selec = lista_ligas[1]
  print(pais_selec)
  times_pais <- lista_times %>% filter(pais == pais_selec) %>% arrange(time)
  
  temp      <- tabela %>% filter(pais == pais_selec) %>% arrange(time)
  temp$time <- normaliza_nome_time(dados = temp$time,lista_times = times_pais)
  temp      <- temp %>% arrange(ano,posicao)
  tabela_limpos <- rbind(tabela_limpos,temp)
}
write.csv(tabela_limpos,'tabela_campeonatos_limpos.csv',row.names = F)

tabela_cbf <- fread('tabela_bra_cbf.csv')
times_pais <- lista_times %>% filter(pais == 'BRA') %>% arrange(time)
tabela_cbf$time <- normaliza_nome_time(dados = tabela_cbf$time,
                                       lista_times = lista_times)
tabela_cbf_limpa <- tabela_cbf %>% arrange(ano,posicao)
write.csv(tabela_cbf_limpa,'tabela_bra_cbf_limpo.csv',row.names = F)

jogos <- fread('jogos_campeonatos.csv')
jogos_limpos <- data.frame()
for (pais_selec in lista_ligas) {
  #pais_selec=lista_ligas[1]
  print(pais_selec)
  times_pais <- lista_times %>% filter(pais == pais_selec) %>% arrange(time)
  
  temp <- jogos %>% filter(pais == pais_selec) #%>% arrange(time_casa)
  #temp <- temp[382:385,]
  
  temp$time_casa <- normaliza_nome_time(dados = temp$time_casa,lista_times = times_pais)
  temp$time_visit <- normaliza_nome_time(dados = temp$time_visit,lista_times = times_pais)
  jogos_limpos <- rbind(jogos_limpos,temp)
}
write.csv(jogos_limpos,'jogos_limpos.csv',row.names = F)

#### PREPARAÇÃO CARTÕES ####
jogos <- fread('jogos_limpos.csv')

jogos <- jogos %>% mutate(ano = ifelse(pais != 'BRA' & month(dmy(data)) < 8,
                                       year(dmy(data,truncated = 2L))-1, year(dmy(data,truncated = 2L))) ) %>%
  filter(!is.na(ano))

time_casa <-  jogos %>%
  select(ano,time_casa,cartao_amarelo_casa,cartao_vermelho_casa,pais)

cartao_casa <- time_casa %>%
  rename(time = time_casa) %>%
  group_by(time,ano) %>%
  summarise(CA = sum(cartao_amarelo_casa,na.rm = T), 
            CV = sum(cartao_vermelho_casa,na.rm = T),
            partida = ifelse(pais == 'BRA',NA,'casa')) %>%
  distinct(ano,time,CA,CV,partida)

time_visit <-  jogos %>%
  select(ano,time_visit,cartao_amarelo_visit,cartao_vermelho_visit,pais)

cartao_visit <- time_visit %>%
  rename(time = time_visit) %>%
  group_by(time,ano) %>%
  summarise(CA = sum(cartao_amarelo_visit,na.rm = T), 
            CV = sum(cartao_vermelho_visit,na.rm = T),
            partida = ifelse(pais == 'BRA',NA,'visitante'))%>%
  distinct(ano,time,CA,CV,partida)

merge_cartao <- rbind(cartao_casa,cartao_visit)
write.csv(merge_cartao,'cartao_times.csv',row.names = F)

## CARTÕES BRASIL CBF ##
tabela_cbf <- fread('tabela_bra_cbf_limpo.csv')

merge_cartao <- merge_cartao %>% #filter(is.na(CA)) %>%
  left_join(.,tabela_cbf,by=c('ano','time')) %>%
  mutate(CA = ifelse(is.na(CA.y),CA.x,CA.y),
         CV = ifelse(is.na(CV.y),CV.x,CV.y)) %>%
  select(colnames(merge_cartao))

merge_cartao <- merge_cartao %>% distinct(ano,time,partida,CA,CV)
write.csv(merge_cartao,'cartao_times.csv',row.names = F)

## inserindo cartoes na tabela de pontos
tabela <- fread('tabela_campeonatos_limpos.csv')
soma_cartao <- merge_cartao %>% group_by(ano,time) %>% 
  summarise(CA = sum(CA,na.rm = T), 
            CV = sum(CV,na.rm = T))

tabela <- tabela %>% left_join(.,soma_cartao, by = c('ano','time'))
write.csv(d,'tabela_campeonatos_limpos.csv',row.names = F)


#### >>> MÉTRICAS <<< ####

tabela <- fread(paste0(DIR_DATA,'tabela_campeonatos_limpos.csv')) %>% data.frame()
cartoes_times <- fread(paste0(DIR_DATA,'cartao_times.csv')) %>% data.frame()
jogos <- fread(paste0(DIR_DATA,'jogos_limpos.csv')) %>% data.frame()
jogos <- jogos %>% filter(ano < 2020) %>% mutate(ano = as.numeric(ano))

#### Calculos gerais(medias) ####
media_gols_ano <-
  jogos %>% 
  mutate(#ano =  as.numeric(ano), #year(dmy(data, truncated = 2L) )   , 
         data =  dmy(data, truncated = 2L)) %>%
  group_by(pais,ano) %>%
  summarise(gols = mean(gols_casa + gols_visit,na.rm = T)) %>% data.frame()
 
 
"
media_gols_ano %>%
  mutate(label = ifelse(ano == max(media_gols_ano$ano,na.rm = T),pais,NA )) %>%
  ggplot(aes(ano,gols))+
    geom_line(aes(ano,gols,color = pais)) +
    scale_x_continuous(labels = media_gols_ano$'ano',breaks = media_gols_ano$'ano')+
  geom_point(aes(ano,gols,color = pais))+
  geom_label_repel(aes(label = label,color = pais),
                   nudge_x = 1,
                   na.rm = TRUE)
" 

#lab_long <- "**<i style='font-size:8pt;color:black;'>Dados disponíveis para o Brasil apenas a partir de 2012</i>"

line_chart(media_gols_ano,title = 'Média Gols por Partida',x_label = 'ANO',y_label = 'Média Gols',
           x = 'ano', y = 'gols',coluna_color = 'pais',label_repel = T,coluna_label_repel = 'pais') 

"
  geom_textbox(aes(x = max(media_gols_ano$ano,na.rm = T)-2, 
                   y = min(media_gols_ano$gols,na.rm = T), label = lab_long), 
               width = unit(15, 'lines'), stat = 'unique') 
"
dados <- tabela
campo <- 'pais'
g <- dados %>%
  group_by(.dots =campo) %>%
  summarise(media_pontos = mean(PTS,na.rm = T), 
            media_gols = mean(PF,na.rm = T),
            media_vitorias = mean(V,na.rm = T),
            media_empates = mean(E,na.rm = T),
            #medias_derrotas = mean(D,na.rm = T),
            media_CA = mean(CA,na.rm = T),
            media_CV = mean(CV,na.rm = T)) %>% as.data.frame()

library(scales)
medias <- cbind(pais=g[,'pais'], data.frame(apply(g[,-1],2,
                                                  function(x) rescale(x, to = c(1,2)))))


## RADAR PLOT
radar_chart(medias,title = 'Médias gerais por campeonato',
            cor = 'pais')

#### Calculos campeoes ####
dados <- tabela %>% filter(posicao == 1)
campo <- 'pais'
medias_campeoes <-dados %>%
                    group_by(.dots = campo) %>%
                    summarise(media_pontos = mean(PTS), 
                              media_gols = mean(PF),
                              media_vitorias = mean(V),
                              media_empates = mean(E),
                              media_derrotas = mean(D))
## RADAR PLOT
radar_chart(medias_campeoes,title = 'Médias gerais campeões por campeonato',
            cor = 'pais')


#### Calculos campeoes por ANO ####
dados <- tabela %>% filter(posicao == 1)
campo <- c('pais','ano')
campeao_ano <- dados %>%
                        group_by(.dots = campo) %>%
                        summarise(pontos = PTS, 
                                  gols = PF,
                                  vitorias = V,
                                  empates = E,
                                  derrotas = D) %>%
                        mutate(ano = ymd(ano, truncated = 2L)) %>% data.frame()

View(dados %>% filter(ano > 2005) %>%
  group_by(time,pais) %>% count()
)
"campeao_ano %>%
ggplot(aes(ano,pontos)) +
  geom_line(aes(color = pais),size = 0.7,alpha = 1) +
  geom_label_repel(aes(label = label,color = label),
                   nudge_x = 1,
 #                  force = 10,
                   na.rm = TRUE)+
  scale_x_date(date_breaks ='1 year',date_labels = '%Y') +
  geom_point(aes(color = pais), size = 1.5) +
  theme_linedraw()
"
d <- melt(campeao_ano, c('pais','ano'))
campeao_ano_gols_pts <- d %>% filter(variable %in% c('gols','pontos'))
line_chart(campeao_ano_gols_pts,title = 'Gols e Pontos Campeões por Ano',
           x_label = 'ANO',y_label = 'GOLS x PTS', x = 'ano', y = 'value',
           coluna_color = 'pais',formula_facet_wrap = "~variable",nrow = 2)


campeao_ano_ved <- d %>% filter(variable %in% c('vitorias'))
line_chart(campeao_ano_ved,title = 'QTD vitórias Campeão por Ano',
           x_label = 'ANO',y_label = 'QTD', x = 'ano', y = 'value',
           coluna_color = 'pais',formula_facet_wrap = "~variable",nrow = 3)


campeao_ano_der <- d %>% filter(variable %in% c('empates','derrotas'))
line_chart(campeao_ano_der,title = 'QTD empates/derrotas Campeão por Ano',
           x_label = 'ANO',y_label = 'QTD', x = 'ano', y = 'value',
           coluna_color = 'pais',formula_facet_wrap = "~variable",nrow = 3)


"
line_chart(campeao_ano,title = 'Gols Campeões por Ano',x_label = 'ANO',y_label = 'Gols',
          x = 'ano', y = 'gols',coluna_color = 'pais',label_repel = T,coluna_label_repel = 'pais')

line_chart(campeao_ano,title = 'Pontos Campeões por Ano',x_label = 'ANO',y_label = 'pontos',
           x = 'ano', y = 'pontos',coluna_color = 'pais',label_repel = T,coluna_label_repel = 'pais')

line_chart(campeao_ano,title = 'Vitórias Campeões por Ano',x_label = 'ANO',y_label = 'Vitórias',
           x = 'ano', y = 'vitorias',coluna_color = 'pais',label_repel = T,coluna_label_repel = 'pais')

line_chart(campeao_ano,title = 'Empates Campeões por Ano',x_label = 'ANO',y_label = 'Empates',
           x = 'ano', y = 'empates',coluna_color = 'pais',label_repel = T,coluna_label_repel = 'pais')

line_chart(campeao_ano,title = 'Derrotas Campeões por Ano',x_label = 'ANO',y_label = 'Derrotas',
           x = 'ano', y = 'derrotas',coluna_color = 'pais',label_repel = T,coluna_label_repel = 'pais')
"
 ## BAR/LINE PLOT



#### Calculos campeoes únicos ####
dados <- tabela %>% filter(posicao == 1)
campo <- c('time','pais')
campeos_unicos <- dados %>%
                    group_by(.dots = campo) %>%
                    count() %>%
                    group_by(pais) %>%
                    count() %>% data.frame()

bar_chart(dados = campeos_unicos,title = 'Campeões Únicos',x_label = 'Liga',
          #coluna_color = 'pais',
          y_label = 'Quantidade', x = 'pais',y = 'n',text_bar=T,coluna_text_bar = 'n')
## BAR PLOT
 

#### Calculos times únicos ####
dados <- tabela 
campo <- c('time','pais')
times_unicos <- dados %>% filter(ano > 2005) %>%
                  group_by(.dots = campo) %>%
                  count() %>%
                  group_by(pais) %>%
                  count() %>% data.frame() 
  

bar_chart(dados = times_unicos,title = 'Times Únicos Participantes',x_label = 'Liga',
          y_label = 'Quantidade', x = 'pais',y = 'n',text_bar=T,coluna_text_bar = 'n')

"
#install.packages('rlang')
library(rlang)

p$mapping$fill
p$mapping$fill <- set_expr(p$mapping$fill, quote(pais))
p$mapping$fill <- set_env(p$mapping$fill, get_env(p$mapping$x))

p
"
## BAR PLOT


#ggplot(dados) +geom_boxplot(aes(pais,PTS),outlier.color = 'red',outlier.alpha = 0.4)

#### Distribuição Gols e PTS ####
d <- melt(dados,c('ano','pais')) %>% mutate(value = as.numeric(value),
                                            variable = as.character(variable)) %>%
  filter(variable %in% c('PTS','PF'))  %>% 
  mutate(variable = ifelse(variable == 'PF','Gols',variable))

boxplot_chart(d,x = 'pais',y='value',title = 'Distribuição dos Gols e PTS',
              formula_facet_wrap = "~ variable")

dados %>% 
  group_by(pais) %>% 
  summarise(CV_GOLS = coef_var(PF), 
            CV_PTS = coef_var(PTS),.groups='drop') %>% 
  data.frame()


#ggplot(dados) +geom_boxplot(aes(pais,PF))
#boxplot_chart(dados,x = 'pais',y='PF',title = 'Distribuição de Gols Marcados',
 #             x_label = 'Liga', y_label = 'Gols Marcados',cor = 'green3')


#ggplot(dados) +geom_boxplot(aes(pais,CA),outlier.color = 'red',fill = 'red',color='white')

#### Distribuição CA ####
boxplot_chart(dados,x = 'pais',y='CA',title = 'Cartão Amarelo',
              x_label = 'Liga', y_label = 'CA',cor = 'yellow')


#ggplot(dados) +geom_boxplot(aes(pais,CV),outlier.color = 'red')
#### Distribuição CV ####
boxplot_chart(dados,x = 'pais',y='CV',title = 'Cartão Vermelho',
              x_label = 'Liga', y_label = 'CV',cor = 'red')


#### Distribuição Gols e PTS e Empates####
point_charts(dados,title = 'Distribuição por Gols x PTS x Empates',
             x = 'PF',y = 'PTS',x_label = 'Gols',y_label = 'PTS',point_color = 'E')


#### Cluster Times ####
dados_cluster <- tabela %>% select(-ano,-time,-pais) %>% filter(!is.na(CA))
set.seed(2020)
cluster <- kmeans(dados_cluster,centers = 5)
dados_cluster$cluster <- cluster$cluster
dados_cluster[,c('time','pais')] <- tabela %>% filter(!is.na(CA)) %>% select(time,pais)

#install.packages('wordcloud2')
library(wordcloud2)
#x <- 1
#rm(df_result)
#df_result <- data.frame()
## calculando médias e wordcloud para cada médias
y <- lapply(sort(unique(cluster$cluster)) ,function(x){
  print(x)
  
  cluster <- dados_cluster %>% filter(cluster == x) 
  medias <- cluster %>%
    select(posicao,V,E,D,PF,PTS,CA,CV) %>% 
    apply(.,2,function(x) mean(x, na.rm = T)) %>% as.array()
  
  result = c(cluster = x,medias, PTS_min = min(cluster$PTS),PTS_max = max(cluster$PTS),
             gols_min = min(cluster$PF),gols_max = max(cluster$PF),
             vit_min = min(cluster$V),vit_max = max(cluster$V))
  
  df_result <<- rbind(df_result,data.frame(t(result)))
  
  dados_word <- dados_cluster %>% filter(cluster == x) %>%
    group_by(time) %>% count() %>% data.frame()
  
  list(result,
       wordcloud2(data = dados_word,color='random-light',
                  #minRotation = -pi/6, minSize = 70,
                  #size = 2,
                  backgroundColor = 'gray3',shuffle = 1)
  )
  #color = lapply(1:20,function(x) tableau_color_pal("Tableau 20")(x))[[20]])
  
})
saveRDS(dados_cluster,'resultado_cluster.RDS')

y[[5]]

y[[3]]
