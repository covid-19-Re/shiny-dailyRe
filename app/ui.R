library(magrittr)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(title = "COVID-19 Reff",
  dashboardHeader(title = HTML("COVID-19 R<sub>effective</sub>")),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem(HTML("R in Switzerland"), tabName = "interactive", icon = icon("chart-area")),
      menuItem(HTML("Additional plots"),
        tabName = "overview", startExpanded = FALSE,
        menuSubItem(HTML("R by canton"), selected = FALSE, tabName = "overview", icon = icon("chart-area")),
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
      tabItem(tabName = "interactive",
        box(title = HTML("Estimating effective R in Switzerland"), width = 12,
          plotlyOutput("CHinteractivePlot", height = "700px")
        ),
        fluidRow(
          column(width = 8,
            box(title = "Methods", width = 12,
              includeMarkdown("md/methodsShort.md"),
              actionLink("methodsLink", "more Details..."))
          ),
          column(width = 4,
            infoBoxOutput("lastUpdateBox", width = 12) %>% withSpinner()
          )
        )
      ),
      tabItem(tabName = "overview",
        fluidRow(
          column(width = 12,
            box(title = HTML("effective R by canton"), width = 12, height = 2600,
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
