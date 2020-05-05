server <- function(input, output, session) {
  # load data
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })

  load(pathToRawData)
  load(pathToEstimatesRePlot)
  load(pathToCantonList)
  lastCheck <- readLines(pathToLastCheck)

  updateSelectInput(session, "canton", choices = cantonList, selected = cantonList)

  observeEvent(input$selectAllCantons, {
    updateSelectInput(session, "canton", selected = cantonList)
  })

  # Render UI
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Switzerland")), tabName = "chPlot", icon = icon("chart-area")),
      menuItem(HTML(i18n()$t("R<sub>e</sub> by canton")), tabName = "cantonsPlot", icon = icon("chart-area")),
      menuItem(i18n()$t("About"), tabName = "about", icon = icon("question-circle")),
      selectInput("lang", i18n()$t("Language"),
        languageSelect, selected = input$lang, multiple = FALSE,
        selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$chPlotUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) in Switzerland")),
        width = 12,
        plotlyOutput("CHinteractivePlot", width = "100%", height = "700px")
      ),
      fluidRow(
        column(width = 8,
          box(width = 12,
            includeMarkdown(str_c("md/methodsShort_", input$lang, ".md"))
            )
        ),
        column(width = 4,
          infoBox(width = 12,
            i18n()$t("Last Data Updates"),
            HTML(dataUpdatesTable(lastDataDateInt(), lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$cantonsPlotUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for cantons")),
      width = 12,
          plotlyOutput("cantonInteractivePlot", width = "100%", height = "700px")
      ),
      fluidRow(
        column(width = 8,
          box(width = 12,
            includeMarkdown("md/methodsShort.md")
            )
        ),
        column(width = 4,
          infoBox(width = 12,
            i18n()$t("Last Data Updates"),
            HTML(dataUpdatesTable(lastDataDateInt(), lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )})

  output$aboutUI <- renderUI({
    includeMarkdown("md/about.md")
  })

  load(pathTolastDataDate)
  lastDataDateInt <- reactive({
    load(pathTolastDataDate)
    lastDataDate$source[1] <- i18n()$t("FOPH")
    return(lastDataDate)
  })

  interventions <- reactive({
    interventions <- read_csv(str_c(pathToInterventionData, "_", input$lang,".csv"),
    col_types = cols(
      name = col_character(),
      y = col_double(),
      text = col_character(),
      tooltip = col_character(),
      type = col_character(),
      date = col_date(format = ""),
      plotTextPosition = col_character()))
    return(interventions)
  })

  cumulativePlotData <- reactive({
    cumulativePlotData <- filter(rawData, !(region != "CH" & data_type == "deaths")) %>%
      mutate(data_type = factor(
        data_type,
        levels = c("confirmed", "hospitalized", "deaths"),
        labels = c("Confirmed cases", "Hospitalized patients", "Deaths"))) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    return(cumulativePlotData)
  })

  cumulativePlotDataFiltered <- reactive({
    cumulativePlotDataFiltered <- cumulativePlotData() %>%
      filter(region %in% input$canton)
    return(cumulativePlotDataFiltered)
  })

  estimatesRePlotFiltered <- reactive({
    estimatesRePlotFiltered <- filter(estimatesRePlot,
      # confirmed: delay 10 days
      !(data_type == "Confirmed cases" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 10)),
      # hospitalized
      !(data_type == "Hospitalized patients" & date > (lastDataDate[lastDataDate$source == "FOPH",]$date - 10)),
      # deaths: delay 16 days
      !(data_type == "Deaths" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 15))
    )
  })

  rEffPlotWindowData <- reactive({
    rEffPlotWindowData <- filter(estimatesRePlotFiltered(),
      estimate_type == "Cori_slidingWindow")
    return(rEffPlotWindowData)
  })

  rEffPlotWindowDataFiltered <- reactive({
    rEffPlotWindowData <- filter(rEffPlotWindowData(),
      region %in% input$canton)
    return(rEffPlotWindowData)
  })
  
  rEffPlotStepData <- reactive({
    rEffPlotStepData <- filter(estimatesRePlotFiltered(),
      estimate_type == "Cori_step")
    return(rEffPlotStepData)
  })

  rEffPlotStepDataFiltered <- reactive({
    rEffPlotStepDataFiltered <- filter(rEffPlotStepData(),
      region %in% input$canton)
    return(rEffPlotStepDataFiltered)
  })

  output$CHinteractivePlot <- renderPlotly({
    plot <- rEffPlotly(
      cumulativePlotData(),
      rEffPlotWindowData(),
      interventions(),
      plotColoursNamed,
      lastDataDate,
      legendOrientation = "v",
      language = input$lang,
      textElements = textElements,
      widgetID = NULL)
    return(plot)
  })

  output$cantonInteractivePlot <- renderPlotly({
    cantonColors <- c(viridis(length(cantonList)-1), "#666666")
    names(cantonColors) <- cantonList

    plot <- rEffPlotlyRegion(
      cumulativePlotData(),
      rEffPlotWindowData(),
      interventions(),
      lastDataDate,
      legendOrientation = "v",
      regionColors = cantonColors,
      language = input$lang,
      textElements = textElements,
      widgetID = NULL)
    return(plot)
  })
}
