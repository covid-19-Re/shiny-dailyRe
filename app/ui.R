library(magrittr)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(title = "COVID-19 Re",
  dashboardHeader(title = HTML("COVID-19 R<sub>e</sub>")),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
      # tabItem(tabName = "chPlot", uiOutput("chPlotUI")),
      # tabItem(tabName = "cantonsPlot", uiOutput("cantonsPlotUI")),
      uiOutput("dashboardBodyUI")
      # tabItem(tabName = "about", uiOutput("aboutUI"))
  )
)
