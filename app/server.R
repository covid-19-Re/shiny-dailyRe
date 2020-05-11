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
  load(pathToEstimatesReSum)
  load(pathToCantonList)
  load(pathToEstimateDates)
  lastCheck <- readLines(pathToLastCheck)

  # Render UI
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Switzerland")), tabName = "chPlot", icon = icon("chart-area")),
      menuItem(HTML(i18n()$t("R<sub>e</sub> by canton")), tabName = "cantonsPlot", icon = icon("chart-area")),
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Europe")),
        lapply(c("Comparison", countryList), function(i) {
          menuSubItem(i, tabName = str_c(str_remove(i, " "), "Plot"), icon = icon("chart-area"))
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
        column(width = 7,
          box(width = 12,
            includeMarkdown(str_c("md/methodsCH_", input$lang, ".md"))
            )
        ),
        column(width = 5,
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
    )
  })

  output$aboutUI <- renderUI({
    includeMarkdown("md/about.md")
  })

  # country UIs
  lapply(countryList, function(i) {
    output[[str_c(str_remove(i, " "), "UI")]] <- renderUI({
      fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) in Europe - ",
          i))),
          width = 12,
          plotlyOutput(str_c(str_remove(i, " "), "Plot"), width = "100%", height = "700px")
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsCountry_", input$lang, ".md"))
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

  output$ComparisonUI <- renderUI({
    fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) in Europe - ",
          "Comparison"))),
          width = 12,
          radioButtons("data_type_select", "Select Data Type to compare",
            choices = c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths"),
            selected = "Confirmed cases", inline = TRUE),
          plotlyOutput("ComparisonPlot", width = "100%", height = "700px")
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsCountry_", input$lang, ".md"))
              )
          ),
          column(width = 4,
            uiOutput("ComparisonDataSourceUI")
          )
        )
      )
  })

  output$ComparisonDataSourceUI <- renderUI({
    infoBox(width = 12,
        i18n()$t("Last Data Updates"),
        HTML(
          dataUpdatesTable(
            latestDataIntComp(),
            lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
        icon = icon("exclamation-circle"),
        color = "purple"
      )
  })

  output$dashboardBodyUI <- renderUI({
    tabList <- c("ch", "cantons", "Comparison", countryList, "about")
    tabs <- lapply(
      tabList,
      function(i) {
        tabItem(tabName = str_c(str_remove(i, " "), "Plot"), uiOutput(str_c(str_remove(i, " " ),"UI")))
      })
    return(do.call(tabItems, tabs))
  })

  load(pathTolatestData)
  latestDataInt <- reactive({
    load(pathTolatestData)
    latestData$source[latestData$source == "FOPH"] <- i18n()$t("FOPH")
    latestDataInt <- latestData
    return(latestDataInt)
  })

  latestDataIntComp <- reactive({
    if(is.null(input$data_type_select)) {
      selectedDataType <- "Confirmed cases"
    } else {
      selectedDataType <- input$data_type_select
    }
    latestDataIntComp <- filter(latestDataInt(), data_type == selectedDataType)
    return(latestDataIntComp)
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

  caseDataSwitzerlandFOPH <- reactive({
    caseDataSwitzerlandFOPH <- rawData %>%
      filter(country == "Switzerland",
        source %in% c("openZH", "FOPH"),
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    return(caseDataSwitzerlandFOPH)
  })

  estimatesSwitzerlandFOPH <- reactive({
    estimatesSwitzerlandFOPH <- estimatesReSum %>%
      filter(country == "Switzerland",
        source %in% c("openZH", "FOPH"),
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      group_by(data_type) %>%
      filter(
        estimate_type == "Cori_slidingWindow",
        between(date,
          left = estimatesDates[["Switzerland"]][["start"]][[as.character(data_type[1])]],
          right = estimatesDates[["Switzerland"]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()
    
    return(estimatesSwitzerlandFOPH)
  })

  caseDataOverview <- reactive({
    caseDataOverview <- rawData %>%
      filter(data_type == input$data_type_select, region %in% countryList,
        !(country == "Switzerland" & source == "ECDC")
      ) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    return(caseDataOverview)
  })

  estimatesOverview <- reactive({
    estimatesOverview <- estimatesReSum %>%
      filter(data_type == input$data_type_select, region %in% countryList,
      !(country == "Switzerland" & source == "ECDC")) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      group_by(country, data_type) %>%
      filter(
        estimate_type == "Cori_slidingWindow",
        between(date,
          left = estimatesDates[["Switzerland"]][["start"]][[as.character(data_type[1])]],
          right = estimatesDates[["Switzerland"]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()

      return(estimatesOverview)
  })

  # country raw data
  caseDataCountry <- lapply(countryList, function(i) {
    rawDataCountry <- rawData %>%
      filter(country == i, region == i) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    if (i == "Switzerland"){
      rawDataCountry <- rawDataCountry %>%
        filter(source != "ECDC")
    }
    return(rawDataCountry)
  })
  names(caseDataCountry) <- str_remove(countryList, " ")

  # country estimates
  estimatesCountry <- lapply(countryList, function(i) {
    estimatesCountry <- estimatesReSum %>%
      filter(country == i, region == i) %>%
      group_by(data_type) %>%
      filter(
        estimate_type == "Cori_slidingWindow",
        between(date,
          left = estimatesDates[[i]][["start"]][[as.character(data_type[1])]],
          right = estimatesDates[[i]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()
    if (i == "Switzerland") {
      estimatesCountry <- estimatesCountry %>%
        filter(source != "ECDC")
    }
    return(estimatesCountry)
  })
  names(estimatesCountry) <- str_remove(countryList, " ")

  output$CHinteractivePlot <- renderPlotly({

    rEffData <- estimatesSwitzerlandFOPH()

    latestDataCH <- filter(latestDataInt(), country == "Switzerland",
      source %in% unique(rEffData$source))

    plot <- rEffPlotly(
      caseDataSwitzerlandFOPH(),
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

    rEffData <- estimatesSwitzerlandFOPH()

    cantonColors <- viridis(length(unique(rEffData$region)))
    names(cantonColors) <- unique(rEffData$region)
    cantonColors["Switzerland"] <- "#666666"

    latestDataCH <- filter(latestDataInt(), country == "Switzerland",
      source %in% unique(rEffData$source))

    plot <- rEffPlotlyRegion(
      caseDataSwitzerlandFOPH(),
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

  output$ComparisonPlot <- renderPlotly({

    caseData <- caseDataOverview()
    focusCountry <- "Switzerland"
    countryColors <- viridis(length(countryList))
    names(countryColors) <- countryList
    countryColors[focusCountry] <- "#666666"

    rEffPlotlyComparison(
      caseData = caseData,
      estimates = estimatesOverview(),
      lastDataDate = latestDataInt(),
      startDate = min(filter(caseData, cumul > 0)$date) - 1,
      focusCountry = focusCountry,
      legendOrientation = "v", # "v" or "h"
      countryColors = countryColors,
      textElements = textElements,
      language = "en-gb",
      widgetID = NULL)

  })

  # country plots
  lapply(countryList, function(i) {
    output[[str_c(str_remove(i, " "), "Plot")]] <- renderPlotly({
      
      latestDataInt <-  latestDataInt()
      if (i == "Switzerland") {
        latestDataInt <- latestDataInt %>%
          filter(source != "ECDC")
      }
      caseData <- caseDataCountry[[str_remove(i, " ")]]

      plot <- rEffPlotlyCountry(
        countrySelect = i,
        caseData = caseData,
        estimates = estimatesCountry[[str_remove(i, " ")]],
        plotColoursNamed = plotColoursNamed,
        lastDataDate = latestDataInt,
        startDate = min(filter(caseData, cumul > 0)$date) - 1,
        legendOrientation = "v", # "v" or "h"
        textElements = textElements,
        language = "en-gb",
        widgetID = NULL)
      return(plot)
    })
  })

}
