library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "COVID-19 R eff"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("chart-area")),
      selectInput("canton", "Cantons", c("ZH","BL","BS","TG", "CH", "All"), selected = "All", multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
      menuItem("Methodology", tabName = "methods", icon = icon("calculator")),
      menuItem("test", tabName = "test", icon = icon("vial")),
      actionButton("goButton", "update calculations now")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        h2("Dashboard"),
        fluidRow(
          column(width = 8,
            box(title = "plot 1", width = 12,
              plotOutput("swissPlot")
            )
          ),
          column(width = 4,
            infoBoxOutput("lastUpdateBox", width = 12),
            box(title = "Methods", width = 12,
              includeMarkdown("md/methodsShort.md"),
              actionLink("methodsLink", "more Details..."))
          ) 
        ),
        fluidRow(
          column(width = 12,
            box(title = "Test Outputs", width = 12,
              textOutput( "test")
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
