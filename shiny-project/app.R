setwd('C:\Users\tata-\Desktop\shiny-project')

source('global.R')
source('ui.R')
source('server.R')


shinyApp(
  ui = ui,
  server = server
)
