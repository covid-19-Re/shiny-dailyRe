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
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Europe")),
        lapply(c("Comparison", countryList), function(i) {
          menuSubItem(i, tabName = str_c(i, "Plot"), icon = icon("chart-area"))
        })
      ),
      menuItem(i18n()$t("About"), tabName = "aboutPlot", icon = icon("question-circle")),
      selectInput("lang", i18n()$t("Language"),
        languageSelect, selected = input$lang, multiple = FALSE,
        selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$chUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) in Switzerland")),
        width = 12,
        plotlyOutput("CHinteractivePlot", width = "100%", height = "700px")
      ),
      fluidRow(
        column(width = 8,
          box(width = 12,
            includeMarkdown(str_c("md/methodsCH_", input$lang, ".md"))
            )
        ),
        column(width = 4,
          infoBox(width = 12,
            i18n()$t("Last Data Updates"),
            HTML(
              dataUpdatesTable(filter(latestDataInt(),
                country == "Switzerland", source %in% c("openZH", "FOPH")),
                lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$cantonsUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for cantons")),
      width = 12,
          plotlyOutput("cantonInteractivePlot", width = "100%", height = "700px")
      ),
      fluidRow(
        column(width = 8,
          box(width = 12,
            includeMarkdown(str_c("md/methodsCH_", input$lang, ".md"))
            )
        ),
        column(width = 4,
          infoBox(width = 12,
            i18n()$t("Last Data Updates"),
            HTML(dataUpdatesTable(filter(latestDataInt(),
              country == "Switzerland", source %in% c("openZH", "FOPH")),
              lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )})

  output$aboutUI <- renderUI({
    includeMarkdown("md/about.md")
  })

  lapply(c("Comparison", countryList), function(i) {
    output[[str_c(i,"UI")]] <- renderUI({
      fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) in Europe - ",
          i))),
          width = 12,
          p("Coming Soon")
          #plotlyOutput("CHinteractivePlot", width = "100%", height = "700px")
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsInt_", input$lang, ".md"))
              )
          ),
          column(width = 4,
            infoBox(width = 12,
              i18n()$t("Last Data Updates"),
              HTML(
                dataUpdatesTable(filter(latestDataInt(), country == i), lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
              color = "purple"
            )
          )
        )
      )
    })
  })

  output$dashboardBodyUI <- renderUI({
    tabList <- c("ch", "cantons", "Comparison", countryList, "about")
    tabs <- lapply(
      tabList,
      function(i) {
        tabItem(tabName = str_c(i, "Plot"), uiOutput(str_c(i,"UI")))
      })
    return(do.call(tabItems, tabs))
  })

  load(pathTolatestData)
  latestDataInt <- reactive({
    load(pathTolatestData)
    latestData$source[latestData$source == "FOPH"] <- i18n()$t("FOPH")
    return(latestData)
  })

  interventions <- reactive({
    interventions <- read_csv(str_c(pathToInterventionData, "_", input$lang, ".csv"),
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

  rawDataCHallCantons <- reactive({
    rawDataCHallCantons <- rawData %>%
      filter(country == "Switzerland",
        source %in% c("openZH", "FOPH"),
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
      mutate(
        region = fct_drop(region),
        country = fct_drop(country),
        data_type = fct_drop(data_type)
      ) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    return(rawDataCHallCantons)
  })

  estimatesRePlotCH <- reactive({
    estimatesRePlotCH <- estimatesRePlot %>%
      filter(country == "Switzerland",
        source %in% c("openZH", "FOPH"),
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths"),
        estimate_type == "Cori_slidingWindow") %>%
      mutate(
        region = fct_drop(region),
        country = fct_drop(country),
        data_type = fct_drop(data_type)
      ) %>%
      filter(
        # confirmed: delay 10 days
        !(data_type == "Confirmed cases" & date > (latestData[latestData$source == "openZH", ]$date - 10)),
        # hospitalized
        !(data_type == "Hospitalized patients" & date > (latestData[latestData$source == "FOPH", ]$date - 10)),
        # deaths: delay 16 days
        !(data_type == "Deaths" & date > (latestData[latestData$source == "openZH", ]$date - 15))
      )
    return(estimatesRePlotCH)
  })

  output$CHinteractivePlot <- renderPlotly({

    rEffData <- estimatesRePlotCH()

    latestDataCH <- filter(latestDataInt(), country == "Switzerland",
      source %in% unique(rEffData$source))

    plot <- rEffPlotly(
      rawDataCHallCantons(),
      rEffData,
      interventions(),
      plotColoursNamed,
      latestDataCH,
      legendOrientation = "v",
      language = input$lang,
      textElements = textElements,
      widgetID = NULL)
    return(plot)
  })

  output$cantonInteractivePlot <- renderPlotly({

    rEffData <- estimatesRePlotCH()

    cantonColors <- c(viridis(length(levels(rEffData$region)) - 1), "#666666")
    names(cantonColors) <- levels(rEffData$region)

    latestDataCH <- filter(latestDataInt(), country == "Switzerland",
      source %in% unique(rEffData$source))

    plot <- rEffPlotlyRegion(
      rawDataCHallCantons(),
      rEffData,
      interventions(),
      latestDataCH,
      legendOrientation = "v",
      regionColors = cantonColors,
      language = input$lang,
      textElements = textElements,
      widgetID = NULL)
    return(plot)
  })
}
