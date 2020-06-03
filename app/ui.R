library(magrittr)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(title = "COVID-19 Re",
  dashboardHeader(title = HTML("COVID-19 R<sub>e</sub>")),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
      uiOutput("dashboardBodyUI")
  )
)
