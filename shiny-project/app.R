setwd('C:/Users/Keaaaaa/Desktop/EstatisticaUFPE/shiny-project/shiny-project')

source('global.R')
source('ui.R')
source('server.R')


shinyApp(
  ui = ui,
  server = server
)
