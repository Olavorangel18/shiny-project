#funções

calcular_moda <- function(v) {
  x <- unique(v)
  x[which.max(tabulate(match(v, x)))]
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  ################### INPUT ####################
  select_day <- eventReactive(input$go, {
    
    day_name <- input$day
    twin <- input$true_date
    
    df_day <- master_df %>% 
      filter(Day == day_name)
    
    df_day1 <- df_day %>% 
      filter(dt >= twin[1] & dt <= twin[2])  
    
    return(df_day1)
  })
  
  select_day_part2 <- eventReactive(input$go_comp, {
    
    day_name <- input$day_comp
    twin <- input$true_date_comp
    
    df_day_input <- master_df %>% 
      filter(Day == day_name[1]) 
    df_day1 <- df_day_input %>% 
      filter(dt >= twin[1] & dt <= twin[2]) 
    
    df_day_input2 <- master_df %>% 
      filter(Day == day_name[2]) 
    df_day2 <- df_day_input2 %>% 
      filter(dt >= twin[1] & dt <= twin[2]) 
    
    df_day_filter = list(df_day1, df_day2)
    return(df_day_filter)
  })
  
  
  
  
  output$timedate <- renderUI({
    
    day_name <- input$day
    
    df <- master_df %>% 
      filter(Day == day_name)
    
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
    
    day_name <- input$day_comp
    
    df <- master_df %>% 
      filter(Day %in% day_name)
    
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
    df <- select_day()
    
    Media <- mean(df$Visits)
    
    x = select(df,Visits)
    Mediana = apply(x,2,median)
    
    
    Moda <- calcular_moda(df$Visits)
    
    DesvioPadrao <- sd(df$Visits)
    
    
    ValorMaximo <- max(df$Visits)
    ValorMinimo <- min(df$Visits)
    
    
    Dia <- input$day
    
    df_tb <-  data.frame(Dia, Media, Moda, Mediana, DesvioPadrao,ValorMaximo,ValorMinimo)
    
    df_tb <- as.data.frame(t(df_tb))
    
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
    df <- select_day()
    
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
    df <- select_day()
    Visitas = df$Visits
    hist(Visitas, main="Histograma de Visitas", col="lightblue")
    
  })
  
  #Segunda Parte
  Info_DataTable_Part2 <- eventReactive(input$go_comp,{
    df <- select_day_part2()
    length1 = length(df[[1]]$Visits)
    length2 = length(df[[2]]$Visits)
    
    if(length1 > length2){
      Correlacao = cor(df[[1]]$Visits[1:length2],df[[2]]$Visits[1:length2])
      Day <- input$day_comp
      df_tb <-  data.frame(Correlacao)
      
      df_tb <- as.data.frame(t(df_tb))
      
      return(df_tb)
    }
    else{
      Correlacao = cor(df[[1]]$Visits[1:length1],df[[2]]$Visits[1:length1])
      Day <- input$day_comp
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
    df <- select_day_part2()
    
    media1 = mean(df[[1]]$Visits)
    media2 = mean(df[[2]]$Visits)
    
    
    labels <- c(input$day_comp[1], input$day_comp[2])
    
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
    df <- select_day_part2()
    aux <- df[[1]]$Visits %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    
    df[[1]]$dt <- ymd(df[[1]]$dt)
    
    auxB <- df[[2]]$Visits %>% na.omit() %>% as.numeric()
    aux3 <- min(auxB)
    aux4 <- max(auxB)
    df[[2]]$dt <- ymd(df[[2]]$dt)

    aux5 <- min(aux1, aux3)
    aux6 <- max(aux2, aux4)
    
    a <- df[[1]] %>% 
      ggplot(aes(dt, Visits, group=1)) +
      geom_path() +
      ylab('Visitas no Website') +
      xlab('Data') +
      coord_cartesian(ylim = c(aux5, aux6)) +
      theme_bw() +
      geom_path() +
      geom_path(data=df[[2]], colour="red")+
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")    
    a
  })
  output$scatter<- renderPlot({
    #all the inputs
    df <- select_day_part2()
    
    length1 = length(df[[1]]$Visits)
    length2 = length(df[[2]]$Visits)
    
    
    if(length1 > length2){
      preco <-  df[[1]]$Visits[1:length2]
      volume <- df[[2]]$Visits[1:length2]
      
    }
    
    else{
      preco <-  df[[1]]$Visits[1:length1]
      volume <- df[[2]]$Visits[1:length1]

      
    }
    
    plot(x = preco, y = volume,
         xlab = input$day_comp[1], 
         ylab = input$day_comp[2],
         pch=19,	 
         main = paste(input$day_comp[1], input$day_comp[2],  sep = " Vs " ))
    
  })
  
  output$boxplot <- renderPlot({
    # All the inputs
    df <- select_day()
    
    Visitas = df$Visits
    
    boxplot(Visitas, data = df,
            main = "Boxplot das visitas")
    
    
    
  })  
  
  
}