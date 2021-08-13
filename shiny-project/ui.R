

header <- dashboardHeader(title = "Projeto de Estatistica")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Metricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Dias', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opcoes', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock', 'Dia', stock_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informacoes sobre as visitas", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Serie de Visitas", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opcoes', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock_comp', 'Dias', stock_list, multiple=TRUE),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),
                fluidRow(
                    box(title = "Informacoes sobre o preco do bitcoin", width = 12, solidHeader = TRUE,
                        DTOutput('info2')
                    )
                ),
                fluidRow(
                    box(title = "Grafico dos precos", width = 12, solidHeader = TRUE,
                        plotOutput('precosLinha')
                    )
                ),
                fluidRow(
                    box(title = "Grafico do volume", width = 12, solidHeader = TRUE,
                        plotOutput('volumeLinha')
                    )
                ),
                fluidRow(
                    box(title = "Comparacao das medias", width = 12, solidHeader = TRUE,
                        plotOutput('barra')
                    )
                ),
                fluidRow(
                    box(title = "Comparacao do precoXvolume", width = 12, solidHeader = TRUE,
                        plotOutput('scatter')
                    )
                ),       
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
