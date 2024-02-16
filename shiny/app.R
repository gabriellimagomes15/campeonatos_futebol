#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd('D:\\Projetos\\analise_campeonatos_fut\\')
source('utils.R')
source('charts.R')

DIR_DATA = 'data/'

#### Lendo Dados ####
tabela <- fread(paste0(DIR_DATA,'tabela_campeonatos_limpos.csv'),
                encoding = 'UTF-8') %>% 
    data.frame() %>% filter(ano > 2005)
#unique(tabela$time)
#cartoes_times <- fread(paste0(DIR_DATA,'cartao_times.csv'),
 #                      encoding = 'UTF-8') %>% data.frame()
jogos <- fread(paste0(DIR_DATA,'jogos_limpos.csv'),
               encoding = 'UTF-8') %>% data.frame() %>% 
    filter(ano < 2020) %>% mutate(ano = as.numeric(ano))

resultado_cluster <- readRDS(paste0(DIR_DATA,'resultado_cluster.RDS'))

#### dashboardHeader ####
cabecalho <- dashboardHeader(title = "Análises Futebol")

#### dashboardSidebar ####
barra_lateral <- dashboardSidebar(#width = '250px', 
                                  sidebarMenu(
                                      menuItem('Dashboard',
                                               tabName = 'dashboard',
                                               icon = icon('dashboard')),
                                      menuItem('Informaçõess',
                                               tabName = 'infos',
                                               icon = icon('info-circle'))
                                  ))

#### dashboardBody ####
painel_principal <- dashboardBody(
    tags$head(tags$style(HTML(css))),
    tabItems(
        tabItem(tabName = 'infos',
                fluidRow(
                    column(width = 12,
                    box(width = '100%',
                        solidHeader = T,
                        status = 'info',
                        title = 'Informativo',
                        background = 'black',
                        h3('Esta aplicação tem como objetivo analisar os dados relacionados a campeonatos de futebol, Brasil, Espanha, Inglaterra e Itália, ela faz
                    parte de uma publicação feita no linkedin, acesso nos links abaixo. 
                    
                    Objetivo inicial aqui é identificar informações úteis sobre estes campeonatos como, média de gols, campeões únicos, cartões amarelos e vermelhos, informaçoes sobre os campeões dos respectivos campeonatos ao longo dos anos e identificação de perfis dos times participantes destes campeonatos utilizando algoritmos de agrupamento (cluster), o KMEANS.'
                        ) , br(),br(),br(),
                    
                        h1("Para mais informações e/ou feedback 
                            entre em contato:"),
                        infoBox(title = 'EMAIL',
                                width = '3',
                                #href = 'https://www.linkedin.com/feed/?trk=',
                                fill = T,
                                color = 'green',
                                icon = icon('google'),
                                subtitle = 'gabriel.lg08@gmail.com'),
                        infoBox(title = '',
                                width = '1',
                                href = 'https://www.linkedin.com/in/gabriellimagomes/',
                                fill = T,
                                color = 'blue',
                                icon = icon('linkedin'),
                                subtitle = ''),
                        infoBox(title = '',
                                width = '1',
                                href = 'https://github.com/gabriellimagomes15',
                                fill = T,
                                color = 'black',
                                icon = icon('github-square'),
                                subtitle = ''),
                        infoBox(title = 'TELEGRAM',
                                width = '3',
                                #href = 'https://github.com/gabriellimagomes15',
                                fill = T,
                                #color = 'black',
                                icon = icon('telegram-plane'),
                                subtitle = '@GabrielLimaGomes')    
                        ),    
                    ),
                    )
                ),
        tabItem(tabName = 'dashboard',
                fluidRow( 
                    
                    valueBox(subtitle = 'Jogos',
                             #color = 'teal',
                             value = format(nrow(jogos), 
                                            big.mark = ".", 
                                            decimal.mark = ',',
                                           scientific = FALSE), 
                             icon = icon("database")),
                    valueBox(subtitle = "Ligas (campeonatos)",
                             #color = 'aqua',
                            value = length(unique(tabela$pais)),
                            icon = icon("futbol")),
                    valueBox(subtitle = "Temporadas",
                             #color = 'red',
                             value = length(unique(tabela$ano)),
                             icon = icon("calendar-alt"))
                ),
                fluidRow(
                    column(width = 8,
                           box(title = 'Filtros',width = '100%',
                               background = 'black',
                               collapsible = T,
                               status  = 'success',
                               column(width = 6,
                                      box(width = '100%',
                                          status = 'success',
                                          #solidHeader = T,
                                          background = 'black',
                                          sliderInput(inputId = 'ano',
                                                      width = '500px',
                                                      label = 'Ano(temporada):',
                                                      min = as.numeric(min(tabela$ano)),
                                                      max = as.numeric(max(tabela$ano)),
                                                      value = c(min(tabela$ano),
                                                                max(tabela$ano)),
                                                      sep = '')
                                      )
                               ),
                               column(width = 6,
                                      box(width = '100%',
                                          status = 'success',
                                          #solidHeader = T,
                                          background = 'black',
                                          selectizeInput(inputId = 'select_pais',
                                                         label = 'País (liga):',
                                                         choices = sort(unique(tabela$pais)),
                                                         multiple = T, options = list(maxItems = 5),
                                                         selected = sort(unique(tabela$pais))),
                                          ),
                                      submitButton("OK")
                               )
                           )## FINAL BOX 
                    ),
                    column(width = 4,
                        infoBox(width = '100%',
                                color = 'orange',
                                value = 'TIMES',
                                title = '',
                                fill = T,
                                subtitle = length(unique(tabela$time)),
                                icon = icon("user-friends")),
                        infoBox(width = '100%',
                                color = 'orange',
                                fill = T,
                                title = '',
                                subtitle = paste0('Média de ',
                                                  round(mean(tabela$PF),2),
                                                  ' gols'),
                                value = 'GOLS',
                                icon = icon("futbol"))
                    )
                ),## FINAL LINHA
                fluidRow(
                    column(width = 12,
                           box(width = '100%',
                               status = 'danger',
                               solidHeader = T,
                               background = 'black',#title = '',  
                               plotlyOutput(outputId = 'gols_partida',
                                            width = '100%')
                           )
                    )
                ),
                fluidRow(
                    column(width = 4,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '1',
                               plotOutput(outputId = 'radar_geral')
                           )
                    ),
                    column(width = 8,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '8',
                               plotlyOutput(outputId = 'boxplot_gols_pts')
                           )
                    )
                ),
                fluidRow(
                    column(width = 4,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '9',
                               plotlyOutput(outputId = 'boxplot_ca')
                           )
                    ),
                    column(width = 4,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '10',
                               plotlyOutput(outputId = 'boxplot_cv')
                           )
                    ),
                    column(width = 4,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '7',
                               plotlyOutput(outputId = 'times_unicos')
                           )
                    )
                ),
                fluidRow(
                    column(width = 12,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '3',
                               plotlyOutput(height = '600px',
                                            outputId = 'gols_pts_campeao')
                           )
                           
                    )
                ),
                fluidRow(
                    column(width = 8,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '4',
                               plotlyOutput(height = '450px',
                                            outputId = 'vitoria_campeao')
                           )
                           
                    ),
                    column(width = 4,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '2',
                               plotOutput(height = '450px',
                                          outputId = 'media_geral_campeao'),
                               
                           )
                           
                    ),
                ),
                fluidRow(
                    column(width = 8,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '5',
                               plotlyOutput(height = '450px',
                                            outputId = 'emp_der_campeao')
                           )
                           
                    ),
                    column(width = 4,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '6',
                               plotlyOutput(height = '450px',
                                            outputId = 'campeao_unico'),
                               
                           )
                           
                    )
                ),
                fluidRow(
                    column(width = 12,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               #title = '11',
                               plotlyOutput(outputId = 'dispersao_gols_pts')
                           )
                           
                    )
                ),
                fluidRow(
                    column(width = 12,
                           box(width = '100%',
                               status = 'danger',
                               #solidHeader = T,
                               background = 'black',
                               collapsible = T,
                               #title = '12-16', 
                               title = 'Cluster Times',
                               #plotlyOutput(outputId = 'y')
                               uiOutput(outputId = 'y')
                           ),
                           #uiOutput(outputId = 'y')
                    ),
                    #uiOutput(outputId = 'y')
                ),
        )
    ), ## fim tabitems
    
)

ui <- dashboardPage(header = cabecalho, 
                    sidebar = barra_lateral,
                    body = painel_principal,
                    #skin = 'red'
                    )
 
# Define server logic required to draw a histogram
server <- function(input, output) {
    options(warn = -1) 
    
    tabela_ <- reactive({
        #print(input$ano)
        tabela %>% filter(ano >= input$ano[1] & ano <= input$ano[2] &
                              pais %in% input$select_pais)
    })
    jogos_ <- reactive({
        #print(input$ano)
        jogos %>% filter(ano >= input$ano[1] & ano <= input$ano[2] &
                              pais %in% input$select_pais)
    })
    
    output$gols_partida <- renderPlotly({
        media_gols_ano <-
            jogos_() %>% 
            mutate(#ano =  as.numeric(ano), #year(dmy(data, truncated = 2L) )   , 
                data =  dmy(data, truncated = 2L)) %>%
            group_by(pais,ano) %>%
            summarise(.groups='drop',gols = mean(gols_casa + gols_visit,na.rm = T)) %>% data.frame()
        
        line_chart(media_gols_ano,title = 'Média Gols por Partida',
                   x_label = 'ANO',y_label = 'Média Gols',
                   x = 'ano', y = 'gols',coluna_color = 'pais') 
        
    })
    
    output$radar_geral <- renderPlot({
        dados <- tabela_() # %>% filter(pais %in% c('BRA',''))
        campo <- 'pais'
        g <- dados %>%
            group_by(.dots =campo) %>%
            summarise(.groups='drop',media_pontos = mean(PTS,na.rm = T), 
                      media_gols = mean(PF,na.rm = T),
                      media_vitorias = mean(V,na.rm = T),
                      media_empates = mean(E,na.rm = T),
                      #medias_derrotas = mean(D,na.rm = T),
                      media_CA = mean(CA,na.rm = T),
                      media_CV = mean(CV,na.rm = T)) %>% as.data.frame()
        
        if( nrow(g) > 1){
            medias <- cbind(pais=g[,'pais'], 
                            data.frame(apply(g[,-1],2,
                                             function(x) rescale(x, to = c(1,2)))
                            )
            )    
        }else{
            medias <- cbind(pais=g[,'pais'], 
                            data.frame(t(apply(t(g[,-1]),2,
                                             function(x) rescale(x, to = c(1,2)))
                            ))
            )
        }
        medias <- medias %>% reshape2::melt(id="pais")
        
        #t(g[,-1])
        #scales::rescale(t(g[,-1]))
        
        #medias <- mutate_at(medias,vars(-pais),function(x) rescale(x,c(1,2)))%>%
         #   reshape2::melt(id="pais")
            
        radar_chart(medias,x = 'variable',y = 'value',
                    title = 'Médias gerais por campeonato',
                    cor = 'pais')
    })
    
    output$boxplot_gols_pts <- renderPlotly({
        dados <- tabela_()
        d <- reshape2::melt(dados,c('ano','pais')) %>% 
            mutate(value = as.numeric(value),
                   variable = as.character(variable)) %>%
            filter(variable %in% c('PTS','PF'))  %>% 
            mutate(variable = ifelse(variable == 'PF','Gols',variable))
        
        boxplot_chart(dados = d,x = 'pais',y='value',
                      title = 'Distribuição dos Gols e PTS',
                      formula_facet_wrap = "~ variable")
    })
    
    output$texto <- renderText({
        tabela_() %>% 
            group_by(pais) %>% 
            summarise(.groups='drop',CV_GOLS = coef_var(PF), 
                      CV_PTS = coef_var(PTS),.groups='drop') %>% 
            data.frame()
    })
    
    output$boxplot_ca <- renderPlotly({
        boxplot_chart(tabela_(),x = 'pais',y='CA',title = 'Cartão Amarelo',
                      x_label = 'Liga', y_label = 'CA',cor = 'yellow')
        
    })
    
    output$boxplot_cv <- renderPlotly({
        boxplot_chart(tabela_(),x = 'pais',y='CV',title = 'Cartão Vermelho',
                      x_label = 'Liga', y_label = 'CV',cor = 'red')
    })
    
    output$times_unicos <- renderPlotly({
        dados <- tabela_() 
        campo <- c('time','pais')
        times_unicos <- dados %>% filter(ano > 2005) %>%
            group_by(.dots = campo) %>%
            count() %>%
            group_by(pais) %>%
            count() %>% data.frame()
        
        bar_chart(dados = times_unicos,title = 'Times Únicos Participantes',
                  x_label = 'Liga',y_label = 'Quantidade', x = 'pais',y = 'n',
                  text_bar=T,coluna_text_bar = 'n')
        
    })
    output$gols_pts_campeao <- renderPlotly({
        dados <- tabela_() %>% filter(posicao == 1)
        campo <- c('pais','ano')
        campeao_ano <- dados %>%
            group_by(.dots = campo) %>%
            summarise(.groups='drop',pontos = PTS, 
                      gols = PF,
                      vitorias = V,
                      empates = E,
                      derrotas = D) %>%
            mutate(ano = ymd(ano, truncated = 2L)) %>% data.frame()
        
        d <- reshape2::melt(campeao_ano, c('pais','ano'))
        campeao_ano_gols_pts <- d %>% filter(variable %in% c('gols','pontos'))
        
        line_chart(campeao_ano_gols_pts,title = 'Gols e Pontos Campeões por Ano',
                   x_label = 'ANO',y_label = 'GOLS x PTS', x = 'ano', y = 'value',
                   coluna_color = 'pais',formula_facet_wrap = "~variable",nrow = 2)
        
    })
    output$vitoria_campeao <- renderPlotly({
        dados <- tabela_() %>% filter(posicao == 1)
        campo <- c('pais','ano')
        campeao_ano <- dados %>%
            group_by(.dots = campo) %>%
            summarise(.groups='drop',pontos = PTS, 
                      gols = PF,
                      vitorias = V,
                      empates = E,
                      derrotas = D) %>%
            mutate(ano = ymd(ano, truncated = 2L)) %>% data.frame()
        
        d <- reshape2::melt(campeao_ano, c('pais','ano'))
        campeao_ano_ved <- d %>% filter(variable %in% c('vitorias'))
        line_chart(campeao_ano_ved,title = 'QTD vitórias Campeão por Ano',
                   x_label = 'ANO',y_label = 'QTD', x = 'ano', y = 'value',
                   coluna_color = 'pais',formula_facet_wrap = "~variable",nrow = 3)
    })
    output$emp_der_campeao <- renderPlotly({
        dados <- tabela_() %>% filter(posicao == 1)
        campo <- c('pais','ano')
        campeao_ano <- dados %>%
            group_by(.dots = campo) %>%
            summarise(.groups='drop',pontos = PTS, 
                      gols = PF,
                      vitorias = V,
                      empates = E,
                      derrotas = D) %>%
            mutate(ano = ymd(ano, truncated = 2L)) %>% data.frame()
        
        d <- reshape2::melt(campeao_ano, c('pais','ano'))
        campeao_ano_der <- d %>% filter(variable %in% c('empates','derrotas'))
        line_chart(campeao_ano_der,title = 'QTD empates/derrotas Campeão por Ano',
                   x_label = 'ANO',y_label = 'QTD', x = 'ano', y = 'value',
                   coluna_color = 'pais',formula_facet_wrap = "~variable",nrow = 3)
    
    })
    output$media_geral_campeao <- renderPlot({
        dados <- tabela_() %>% filter(posicao == 1)
        campo <- 'pais'
        medias_campeoes <-dados %>%
            group_by(.dots = campo) %>%
            summarise(.groups='drop',media_pontos = mean(PTS), 
                      media_gols = mean(PF),
                      media_vitorias = mean(V),
                      media_empates = mean(E),
                      media_derrotas = mean(D))
        
        if( nrow(medias_campeoes) > 1){
            medias_campeoes <- mutate_at(medias_campeoes,vars(-pais),
                                         function(x) rescale(x,c(1,2)))    
        }else{
            medias_campeoes <- cbind(pais=medias_campeoes[,'pais'], 
                            data.frame(t(apply(t(medias_campeoes[,-1]),2,
                                               function(x) rescale(x, to = c(1,2)))
                            ))
            )
        }
        #medias_campeoes <- mutate_at(medias_campeoes,vars(-pais),
         #                            function(x) rescale(x,c(1,2)))%>%
          #  melt(id="pais")
        medias_campeoes <- medias_campeoes %>% reshape2::melt(id="pais")
        
        ## RADAR PLOT
        radar_chart(medias_campeoes,x = 'variable',y='value',
                    title = 'Médias gerais campeões por campeonato',
                    cor = 'pais')
        
    })
    output$campeao_unico  <- renderPlotly({
        dados <- tabela_() %>% filter(posicao == 1)
        campo <- c('time','pais')
        campeos_unicos <- dados %>%
            group_by(.dots = campo) %>%
            count() %>%
            group_by(pais) %>%
            count() %>% data.frame()
        
        bar_chart(dados = campeos_unicos,title = 'Campeões Únicos',x_label = 'Liga',
                  y_label = 'Quantidade', x = 'pais',y = 'n',
                  text_bar=T,coluna_text_bar = 'n')
    }) 
    output$dispersao_gols_pts  <- renderPlotly({
        point_charts(tabela_(),title = 'Distribuição por Gols x PTS x Empates',
                     x = 'PF',y = 'PTS',x_label = 'Gols',y_label = 'PTS',
                     point_color = 'E')
        
    })
     
    ## PLOT CLUSTER ##
    lapply(1:5, function(x) {
        isolate({
            cluster <- resultado_cluster %>% filter(cluster == x) 
            
            medias <- cluster %>%
                select(posicao,V,E,D,PF,PTS,CA,CV) %>% 
                apply(.,2,function(x) round(mean(x, na.rm = T),2)) %>% 
                t() %>%
                as.data.frame()
            
            result = data.frame(cluster = x,medias, PTS_min = min(cluster$PTS),PTS_max = max(cluster$PTS),
                       gols_min = min(cluster$PF),gols_max = max(cluster$PF),
                       vit_min = min(cluster$V),vit_max = max(cluster$V))
            
            dados_word <- resultado_cluster %>% filter(cluster == x) %>%
                group_by(time) %>% count() %>% data.frame()
            
            insertUI(
                selector = '#y',
                #where = "afterEnd",
                ui = box(background = 'black', 
                         status = 'info',
                        plotOutput(outputId = paste0('cities',x)),
                          DT::dataTableOutput(paste0('tab',x))
                ),
                 #ui = wordcloud2Output(outputId = paste0('cities',i))  
            )
            output[[paste0('tab',x)]] <- DT::renderDataTable({
                DT::datatable(result,rownames = FALSE, 
                              options = list(paging = FALSE,
                                             searching = FALSE,
                                             scrollX = TRUE))
                
            })
            
            output[[paste0('cities',x)]] <- 
                renderPlot({
                    #cluster[[i]][[2]]
                    par(bg="#333333",mar = rep(0, 4)) 
                    
                    wordcloud(dados_word$time,
                              dados_word$n,
                              min.freq = 2,
                              max.words = 300,
                              random.order = F,
                              random.color = F,
                              #scale = c(1,3),
                              colors=tableau_color_pal("Tableau 20")(20))
 
            })
            
            #output$cities <- 
 
        })
    })
 
    
}

# Run the application 
shinyApp(ui = ui, server = server)




