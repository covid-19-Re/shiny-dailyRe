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
        uiOutput("plotUI"),
        fluidRow(
          column(width = 8,
            uiOutput("methodsUI")
          ),
          column(width = 4,
            uiOutput("dataSourceUI")
          )
        )
      ),
      tabItem("about",
        uiOutput("aboutUI")
      )
    )
  )
)
