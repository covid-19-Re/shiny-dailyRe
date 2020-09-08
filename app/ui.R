library(magrittr)
library(shinydashboard)

dashboardPage(title = "COVID-19 Re",
  dashboardHeader(
    title = HTML("COVID-19 R<sub>e</sub>"),
    titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem("plots",
        fluidRow(
          column(4, uiOutput("avgR")),
          column(4, uiOutput("currentR"))
        ),
        fluidRow(column(12,
          uiOutput("timeseriesPlotUI"),
        )),
        fluidRow(
          uiOutput("methodsUI"),
          uiOutput("dataSourceUI")
        )
      ),
      tabItem("mapPlot",
        fluidRow(
          uiOutput("mapPlotUI")
        ),
        fluidRow(
          uiOutput("mapMethodsUI"),
          box(width = 4, title = "Color scale options",
            plotlyOutput("mapHist", height = "250px"),
            conditionalPanel(
              condition = "input.mapPlot_groups == \"Cases / 100'000 / 14 d\"",
              div(
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("casesMidpoint", "Breakpoint", value = 60,
                    min = 0, max = 1500, step = 1)
                ),
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("casesCutoff", "Cutoff",
                    value = 300,
                    min = 0, max = 1500, step = 50)
                )
              )
            ),
            conditionalPanel(
              condition = "input.mapPlot_groups == 'median Re'",
              div(
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("reMidpoint", "Breakpoint", value = 1,
                    min = 0, max = 20, step = 0.1)
                ),
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("reCutoff", "Cutoff",
                    value = 2,
                    min = 0, max = 20, step = 0.1)
                )
              )
            )
          )
        )
      ),
      tabItem("about",
          uiOutput("aboutUI")
      )
    )
  )
)
