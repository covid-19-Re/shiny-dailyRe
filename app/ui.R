library(magrittr)
library(shinydashboard)
library(shinycssloaders)


dashboardPage(title = "COVID-19 Reff",
  dashboardHeader(title = HTML("COVID-19 R<sub>effective</sub>")),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem(HTML("Estimating R<sub>effective</sub>"),
        tabName = "overview", icon = icon("chart-area"), startExpanded = TRUE,
        menuSubItem("Options", selected = TRUE, tabName = "overview", icon = icon("cogs")),
        selectInput("canton", "Cantons", choices = "CH", selected = "CH",
          multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
        actionLink("selectAllCantons", "select all")
      ),
      menuItem("Methodology", tabName = "methods", icon = icon("calculator")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        h2(HTML("Estimating R<sub>effective</sub>")),
        fluidRow(
          column(width = 9,
            box(title = "Methods", width = 12,
              includeMarkdown("md/methodsShort.md"),
              actionLink("methodsLink", "more Details..."))
          ),
          column(width = 3,
            infoBoxOutput("lastUpdateBox", width = 12) %>% withSpinner()
          )
        ),
        fluidRow(
          column(width = 12,
            box(title = HTML("effective R<sub>0</sub>"), width = 12, height = 2600,
              fluidRow(
                column(width = 4,
                  plotOutput("cumulativePlot") %>% withSpinner()
                ),
                column(width = 4,
                  plotOutput("rEffPlotWindow") %>% withSpinner()
                ),
                column(width = 4,
                  plotOutput("rEffPlotStep") %>% withSpinner()
                )
              )
            )
          )
        )
      ),
      tabItem(tabName = "methods",
        includeMarkdown("md/methodsLong.md")
      ),
      tabItem(tabName = "about",
        includeMarkdown("md/about.md")
      )
    )
  )
)
