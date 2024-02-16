#install.packages('rvest')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('plotly')
#install.packages('forcats')
#install.packages('ggrepel')
#install.packages('ggtext')
#install.packages('ggthemes')
#install.packages('hrbrthemes')
#install.packages('colorspace')


#install.packages('ggiraphExtra')
#install.packages('ggiraph')
#install.packages('https://cran.r-project.org/src/contrib/Archive/ggiraphExtra/ggiraphExtra_0.2.9.tar.gz')

#devtools::install_version("ggiraphExtra",version = '0.2.9',dependencies = T)
#devtools::install_github("cardiomoon/ggiraphExtra",dependencies = T)

#require(ggiraphExtra)
#require(ggiraph)
#library(fmsb)

#library(rstudioapi)
#install.packages("shinythemes")
#install.packages('fmsb')
#install.packages('DT')

#install.packages('evaluate')

library(colorspace)
#library(rvest)
#library(ggrepel)
#library(hrbrthemes)
#library(fmsb)
#library(shinythemes)
#library(ggtext)

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
#library(shinyWidgets)
library(shinydashboard)

library(wordcloud)
library(forcats)
library(ggthemes)
#library(evaluate)


## função para normalizar nome dos times com base na lista "lista_times"
normaliza_nome_time <- function(dados,lista_times){
  dados <- iconv(dados, to = "ASCII//TRANSLIT")
  lista_times$time <- iconv(lista_times$time, to = "ASCII//TRANSLIT")
  retorno <- data.frame()
  for(idx in 1:length(dados)){
    continua = T
    j = 1
    while(continua){
      pattern_time <- lista_times[j,]
      time <- dados[idx]
      if(grepl(pattern = pattern_time$time,x = time)){
        dados[idx] <- pattern_time$time_clean
        continua <- F
      }
      if(j == nrow(lista_times)){
        continua <- F
      }
      j = j+1
    }
  }
  return(dados)
}

## função PARA CALCULAR O COEFCICIENTE DE VARIAÃÃO
coef_var <- function(dados){
  get_cv <- function(x){
    sd <- sd(x,na.rm = T)
    mean <- mean(x,na.rm = T)
    cv <- (sd/mean)
    return(cv)
  }
  
  if(class(dados) == 'data.frame'){
    cv <- apply(dados,2,function(x){
            options(warn = -1) 
            x <- as.numeric(x)
            if(class(x) %in% c('integer','numeric','double')){
              cv <- get_cv(x)
            }
            cv
          })  
  }else{
    cv <- get_cv(dados)
  }
  
  return(cv)
}
 

#tags$head(tags$style(HTML(
css <- '
                                
 
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: black;
                                }
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #4e79a7;
                                }
                                
                                .bg-black>.box-body{
                                background-color: #333;
                                }
                                
                                 /* logo */
                                .skin-blue .main-header .logo 
                                {
                                background-color: #f4f4f4;
                                color: #333
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #f4f4f4;
                                }
                                .skin-blue .main-header .navbar .sidebar-toggle {
                                color: #333;
                                }
                                
      
                                .small-box .bg-aqua{
                                background-color: #76B7B2!important
                                }
                                
                                
                                .info-box .bg-orange{
                                background-color: #f28e2b!important
                                }
                                
                                .bg-red>.info-box-content{
                                color: black;
                                }
                                
                                th.dt-right{
                                color: #ffffff;
                                }
                                
                                '#))),


