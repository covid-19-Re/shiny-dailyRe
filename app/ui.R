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
        fluidRow(column(12,
          uiOutput("plotUI"),
        )),
        fluidRow(
            uiOutput("methodsUI"),
            uiOutput("dataSourceUI")
        )
      ),
      tabItem("mapPlot",
        fluidRow(
          uiOutput("mapPlotUI")
        )
      ),
      tabItem("about",
          uiOutput("aboutUI")
      )
    )
  )
)
