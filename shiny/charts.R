#theme_set(theme_linedraw())
#theme_set(theme_modern_rc(axis_title_size = 10))

#https://jrnold.github.io/ggthemes/reference/scale_colour_gradient_tableau.html
#https://shiny.rstudio.com/articles/dynamic-ui.html
#https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/
#https://stackoverflow.com/questions/52905712/observeevent-in-insertui-generated-in-loop

style_chart <- function(plot){
  plot <- plot + theme_base()+#theme_modern_rc(axis_title_size = 10,axis_text_size = 12)+
    theme(
          panel.grid.major = element_line(color = "gray30", size = 1,
                                          linetype = 'solid'),
          #panel.grid.minor = element_line(color = "yellow", size = .5,
           #                               linetype = "dashed"),
      line = element_line(color = "white", size = 1,
                              linetype = 2, lineend = "butt"),
          rect = element_rect(fill = "#333333", color = "gray50",
                              size = 1, linetype = 1),
          text = element_text( face = "plain",
                              color = "white", size = 12,
                              lineheight = 5, hjust = .5, vjust = .5,
                              angle = 0, margin = margin(), debug = FALSE)
          )
  return(plot)
}

line_chart <- function(dados, title = '', x = '',y='',coluna_color=NULL,
                       x_label = '', y_label = '', label_repel = F,coluna_label_repel = NA,
                       date_labels = '%Y',formula_facet_wrap = NA,nrow = NULL,ncol = NULL){
  
  #if(label_repel & coluna_label_repel != ''){
  if( !is.na(coluna_label_repel) & coluna_label_repel != ''){
    dados$label <- apply(dados,1,function(x){
      ifelse(x['ano'] == max(dados$ano,na.rm = T),x[coluna_color],NA)
    })
  }
  
  p <- ggplot(data = dados, aes_string(x,y,color = coluna_color)) +
    geom_line()+
    #geom_point(aes_string(x,y,color = coluna_color))
    geom_point()
  
  if(label_repel & coluna_label_repel != ''){
    print('if')
    p <- p + geom_label_repel(aes_string(label = 'label',color = coluna_color),
                              nudge_x = 1,
                              na.rm = TRUE)
  }
  
  p <- p + ggtitle(title) + xlab(x_label) + ylab(y_label) + scale_color_tableau()
  
  if ( class(dados[,x]) =='numeric'){
    p <- p + scale_x_continuous(labels = dados[,x],breaks = dados[,x])
  }else if (class(dados[,x]) =='Date') {
    p <- p + scale_x_date(date_breaks ='1 year',date_labels = date_labels)
  }
  if(!is.na(formula_facet_wrap)){
    p <- p + facet_wrap(as.formula(formula_facet_wrap),
                        nrow = nrow,ncol = ncol,scales = 'free_x')
  }
  p <- style_chart(p)
  return(ggplotly(p))
  #return(p)
}


bar_chart <- function(dados, title = '', x = '',y='',
                      coluna_color=tableau_color_pal("Tableau 20")(1),
                      x_label = '', y_label = '', text_bar = F,coluna_text_bar = NA,
                      cor_barra = 'blue',formula_facet_wrap = NA,nrow = NULL,ncol = NULL){
  #x = 'time'
  #y = 'PTS'
  p <- dados %>%
    ggplot(aes_string(x=fct_reorder(.[[x]],.[[y]],.desc = T),y=y))
  
  if(coluna_color %in% colnames(dados)){
    p <- p + geom_bar(stat = 'identity', 
             aes(fill = dados[,coluna_color],color=dados[,coluna_color],
                 text = paste0('Pais:',dados[,x],'\nQTD:',dados[,y]) ))
  }else{
    p <- p + geom_bar(stat = 'identity',fill = coluna_color, 
             aes(text = paste0('Pais:',dados[,x],'\nQTD:',dados[,y]) ))
  }
  
  if(F & text_bar & !is.na(coluna_text_bar)){
    print('IF')
    p <- p + geom_text(aes_string(label = coluna_text_bar),vjust = -0.1)
  }
  if(!is.na(formula_facet_wrap)){
    p <- p + facet_wrap(as.formula(formula_facet_wrap),
                        nrow = nrow,ncol = ncol,scales = 'free_x')
  }
  p <- p + ggtitle(title) + xlab(x_label) + ylab(y_label) + 
    scale_fill_tableau() + scale_color_tableau()
    #scale_fill_manual(values = cor_barra)
  
  #p <- p +   scale_fill_manual(values = cor_barra)

  #return(ggplotly(p) %>% style(text = dados[,coluna_text_bar], textposition = "auto"))
  p <- style_chart(p)
  return(ggplotly(p,tooltip = 'text') %>% hide_guides() )
}


boxplot_chart <- function(dados, title = '', x = '',y='',cor = 'white',
                          x_label = '', y_label = '',
                          formula_facet_wrap = NA,nrow = NULL,ncol = NULL){
  
  p <- ggplot(dados,aes_string(x,y)) +
        geom_boxplot(aes(color=dados[,x],
                                fill = after_scale(desaturate(lighten(color, .4), .6)),
                     ),
                     size = 0.7,
                     outlier.size = 2,
                     )
  
  p <- p + ggtitle(title) + xlab(x_label) + ylab(y_label)+
    scale_color_tableau(guide = 'none')
  #+# brewer(palette = "Dark2", guide = "none")
  if(!is.na(formula_facet_wrap)){
    p <- p + facet_wrap(as.formula(formula_facet_wrap),
                        nrow = nrow,ncol = ncol,scales = 'free_x')
  }
  p <- ggplotly(style_chart(p)) %>% hide_guides()
  
  p$x$data <- lapply(p$x$data, FUN = function(x){
    x$marker = list(opacity = 0.5)
    x$marker$outliercolor = "cyan"
    x$marker$color = "cyan"
    
    return(x)
  })
  
  return( p)
}

point_charts <- function(dados, title = '', x = '',y='',point_color=NULL,
                         x_label = '', y_label = '', text_bar = F,coluna_text_bar = NA,
                         cor_barra = 'blue',
                         formula_facet_wrap = NA,nrow = NULL,ncol = NULL){
  
  p <- dados %>% 
    ggplot(aes_string(x,y,color = point_color)) +
    geom_point(alpha = 0.4)
  
  p <- p + ggtitle(title) + xlab(x_label) + ylab(y_label)+
    scale_color_gradient_tableau(palette = 'Blue-Green Sequential')
  
  if(!is.na(formula_facet_wrap)){
    p <- p + facet_wrap(as.formula(formula_facet_wrap),
                        nrow = nrow,ncol = ncol,scales = 'free_x')
  }
  p <- style_chart(p)
  #return(ggplotly(style_chart(p)))
  return(ggplotly(p))
}


radar_chart_OLD <- function(dados, title = '', x = '',y='',
                        x_label = '', y_label = '',
                        cor = NULL){
  
  p <- ggRadar(data=dados,aes_string(colour=cor),
               interactive=F,ylim = -.5,
               use.label = F)+
    ggtitle(title) + xlab(x_label) + ylab(y_label)+
    scale_fill_tableau()+scale_color_tableau()
  
  return(style_chart(p)+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 #plot.margin=margin(50,50,50,50)
                 )
  )
}

radar_chart <- function(dados, title = '', x = '',y='',
                        x_label = '', y_label = '',
                        cor = NULL){
  #dados <- medias
  p <- ggplot(data=dados,aes_string(x = x,
                                    y = y,
                                    group = cor,
                                    fill = cor,
                                    colour=cor)) +
                geom_polygon(alpha=0.15) +
                geom_point(size = 2)+
                coord_radar() +
                ylim(c(-.5,2))+
                scale_fill_tableau()+scale_color_tableau()+
    ggtitle(title) + xlab(x_label) + ylab(y_label)
    
  return(style_chart(p)+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 #plot.margin = margin(rep(6,4), "cm"),
           ) 
         )
}

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}
