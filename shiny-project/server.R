#funcções

calcular_moda <- function(v) {
    x <- unique(v)
    x[which.max(tabulate(match(v, x)))]
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_stock <- eventReactive(input$go, {
        
        stock_name <- input$stock
        twin <- input$true_date
        
        df_stock <- master_df %>% 
            filter(Day == stock_name)
        
        df_stock1 <- df_stock %>% 
            filter(dt >= twin[1] & dt <= twin[2])  
   
        return(df_stock1)
    })

    select_stock_part2 <- eventReactive(input$go_comp, {
      
      stock_name <- input$stock_comp
      twin <- input$true_date_comp
      
      df_stock_input <- master_df %>% 
        filter(Day == stock_name[1]) 
      df_stock1 <- df_stock_input %>% 
        filter(dt >= twin[1] & dt <= twin[2]) 
      
      df_stock_input2 <- master_df %>% 
        filter(Day == stock_name[2]) 
      df_stock2 <- df_stock_input2 %>% 
        filter(dt >= twin[1] & dt <= twin[2]) 
      
      df_stock_filter = list(df_stock1, df_stock2)
      return(df_stock_filter)
    })



    
    output$timedate <- renderUI({
        
        stock_name <- input$stock
        
        df <- master_df %>% 
            filter(Day == stock_name)
        
        min_time <- min(df$dt)
        max_time <- max(df$dt)
        dateRangeInput("true_date", "Periodo de analise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate_comp <- renderUI({
        
        stock_name <- input$stock_comp
        
        df <- master_df %>% 
            filter(Day %in% stock_name)
        
        maxmin_time <- df %>% 
            group_by(Day) %>% 
            summarise(MD = min(dt)) %>% 
            .$MD %>% 
            max()
        
        minmax_time <- df %>% 
            group_by(Day) %>% 
            summarise(MD = max(dt)) %>% 
            .$MD %>% 
            min()
        
        min_time <- maxmin_time
        max_time <- minmax_time
        
        dateRangeInput("true_date_comp", "Periodo de analise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_stock()
        
        Media <- mean(df$Visits)
        
        x = select(df,Visits)
        Mediana = apply(x,2,median)

        
        Moda <- calcular_moda(df$Visits)

        DesvioPadrao <- sd(df$Visits)

        
        ValorMaximo <- max(df$Visits)
        ValorMinimo <- min(df$Visits)

      
        Dia <- input$stock
        
        df_tb <-  data.frame(Dia, Media, Moda, Mediana, DesvioPadrao,ValorMaximo,ValorMinimo)
        
        df_tb <- as.data.frame(t(df_tb))
        
        # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
        # tb <- tb %>% 
        #     rename('Informações' = nms,
        #            'Valores' = V2)
        # 
        return(df_tb)
    })
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
    output$sh <- renderPlot({
        # All the inputs
        df <- select_stock()
        
        aux <- df$Visits %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$dt <- ymd(df$dt)
        a <- df %>% 
            ggplot(aes(dt, Visits, group=1)) +
            geom_path() +
            ylab('Visitas no Website') +
            xlab('Data') +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
    })
    
    output$h <- renderPlot({
            # All the inputs
            df <- select_stock()
            Visitas = df$Visits
            hist(Visitas, main="Histograma de Visitas", col="lightblue")
            
        })

 #Segunda Parte
    Info_DataTable_Part2 <- eventReactive(input$go_comp,{
      df <- select_stock_part2()
      length1 = length(df[[1]]$Visits)
      length2 = length(df[[2]]$Visits)

      if(length1 > length2){
         Correlacao = cor(df[[1]]$Visits[1:length2],df[[2]]$Visits[1:length2])
         Stock <- input$stock_comp
         df_tb <-  data.frame(Correlacao)

         df_tb <- as.data.frame(t(df_tb))
        
         return(df_tb)
      }
      else{
         Correlacao = cor(df[[1]]$Visits[1:length1],df[[2]]$Visits[1:length1])
         Stock <- input$stock_comp
         df_tb <-  data.frame(Correlacao)

         df_tb <- as.data.frame(t(df_tb))
         return(df_tb)
      }
      
    })

output$info2 <- renderDT({
  Info_DataTable_Part2() %>%
    as.data.frame() %>%
    DT::datatable(options=list(
      language=list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
      )
    ))
})


output$barra <-renderPlot ({
  df <- select_stock_part2()
  
  media1 = mean(df[[1]]$Visits)
  media2 = mean(df[[2]]$Visits)
  

  labels <- c(input$stock_comp[1], input$stock_comp[2])
  
  values <- c(media1, media2)
  
  
  data <- data.frame(
    name=labels,  
    value=values
  )
  a <- ggplot(data, aes(x=name, y=value)) + 
    geom_bar(stat = "identity")
  a
  
})

output$doublesh <- renderPlot({
        # All the inputs
        df <- select_stock_part2()
        aux <- df[[1]]$Visits %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df[[1]]$dt <- ymd(df[[1]]$dt)
        
        auxB <- df[[2]]$Visits %>% na.omit() %>% as.numeric()
        aux3 <- min(auxB)
        aux4 <- max(auxB)
        df[[2]]$dt <- ymd(df[[2]]$dt)
        
        a <- df[[1]] %>% 
            ggplot(aes(dt, Visits, group=1)) +
            geom_path() +
            ylab('Visitas no Website') +
            xlab('Data') +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            geom_path() +
            geom_path(data=df[[2]], colour="red")+
            coord_cartesian(ylim = c(aux3, aux4)) +
            theme_bw()    
        a
    })

}