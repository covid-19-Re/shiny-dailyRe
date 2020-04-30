library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "COVID-19 R eff"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(HTML("effective R<sub>0</sub>"), tabName = "overview", icon = icon("chart-area")),
      selectInput("canton", "Cantons", choices = "CH", selected = "CH",
        multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
      actionLink("selectAllCantons", "select all"),
      menuItem("Methodology", tabName = "methods", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        h2(HTML("Estimation of effective R<sub>0</sub>")),
        fluidRow(
          column(width = 3,
            infoBoxOutput("lastUpdateBox", width = 12)
          ),
          column(width = 9,
            box(title = "Methods", width = 12,
              includeMarkdown("md/methodsShort.md"),
              actionLink("methodsLink", "more Details..."))
          )
        ),
        fluidRow(
          column(width = 12,
            box(title = HTML("effective R<sub>0</sub>"), width = 12, height = 2500,
              fluidRow(
                column(width = 4,
                  plotOutput("cumulativePlot")
                ),
                column(width = 4,
                  plotOutput("rEffPlotWindow")
                ),
                column(width = 4,
                  plotOutput("rEffPlotStep")
                )
              )
            )
          )
        )    
      ),
      tabItem(tabName = "methods",
        h2("Methods"),
        includeMarkdown("md/methodsLong.md")
      )
    )
  )
)
