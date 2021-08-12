

header <- dashboardHeader(title = "Projeto de Estatistica")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Metricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Acoes', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opcoes', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock', 'Acao', stock_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informacoes sobre a acao", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Serie de Precos", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opcoes', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock_comp', 'Acao', stock_list, multiple=TRUE),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),            
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
