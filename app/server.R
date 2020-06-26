server <- function(input, output, session) {

  stateVals <- reactiveValues(lang = "en-gb", tabs = "ch", sidebarExpanded = "chMenu")

  # record state on language change
  observeEvent(input$lang, {
      stateVals$lang <- input$lang
      stateVals$tabs <- input$tabs
      stateVals$sidebarExpanded <- input$sidebarItemExpanded
  })

  # load data
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    # restore selected tabs
    updateTabItems(session, "tabs", selected = stateVals$tabs)
    return(translator)
  })

  load(pathToRawData)
  load(pathToEstimatesReSum)
  load(pathToEstimateDates)
  load(pathToValidEstimates)
  load(pathTolatestData)
  load(pathToPopSizes)
  lastCheck <- readLines(pathToLastCheck)

  # Render UI
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Switzerland")),
        expandedName = "chMenu", startExpanded = stateVals$sidebarExpanded == "chMenu",
        menuSubItem(HTML(i18n()$t("Switzerland")), tabName = "ch", icon = icon("chart-area")),
        menuSubItem(HTML(i18n()$t("R<sub>e</sub> by canton")), tabName = "cantons", icon = icon("chart-area")),
        menuSubItem(HTML(i18n()$t("R<sub>e</sub> for greater Regions")),
          tabName = "greaterRegions", icon = icon("chart-area"))
      ),
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Europe")),
        expandedName = "euMenu", startExpanded = stateVals$sidebarExpanded == "euMenu",
        lapply(c("Comparison", countryList), function(i) {
          menuSubItem(i, tabName = str_remove(i, " "), icon = icon("chart-area"))
        })
      ),
      menuItem(HTML(i18n()$t("Download R<sub>e</sub> estimates")),
                  tabName = "download", icon = icon("download")),
      menuItem(i18n()$t("About"), tabName = "about", icon = icon("question-circle")),
      uiOutput("plotOptions"),
      selectInput("lang", i18n()$t("Language"),
        languageSelect, selected = input$lang, multiple = FALSE,
        selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$plotOptions <- renderUI({
      div(style = "background-color:#2F3B41; margin:15px; border-radius:5px; padding:10px",
        h4("Plot Options", style = "margin-top:0px"),
        HTML("<i>Plot 1 - Case Data</i>"),
        div(style = "margin-left:10px !important;",
          checkboxInput("logCases", "Logarithmic axis for cases", FALSE),
          checkboxInput("caseNormalize", "Normalize cases to per 100'000 inhabitants", FALSE),
          radioButtons("caseAverage", "Display case data as ...",
            choices = c("daily case numbers" = 1, "7-day average" = 7),
            selected = 1, inline = FALSE)
        ),
        HTML("<i>Plot 2 - R<sub>e</sub> estimates</i>"),
        div(style = "margin-left:10px !important; margin-top:10px",
          radioButtons("estimation_type_select", "Select estimation type to show",
            choices = c("sliding window" = "Cori_slidingWindow", "step-wise constant" = "Cori_step"),
            selected = "Cori_slidingWindow", inline = FALSE)
        )
      )
  })

  output$chUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) in Switzerland")),
        width = 12,
        plotlyOutput("CHinteractivePlot", width = "100%", height = "800px")
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
              dataUpdatesTable(filter(latestData,
                country == "Switzerland", source %in% c("FOPH")),
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
          plotlyOutput("cantonInteractivePlot", width = "100%", height = "800px")
      ),
      fluidRow(
        column(width = 7,
          box(width = 12,
            includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
            )
        ),
        column(width = 5,
          infoBox(width = 12,
            i18n()$t("Last Data Updates"),
            HTML(dataUpdatesTable(filter(latestData,
              country == "Switzerland", source %in% c("FOPH")),
              lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$greaterRegionsUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for greater regions of Switzerland")),
        width = 12,
        plotlyOutput("greaterRegionInteractivePlot", width = "100%", height = "800px")
      ),
      fluidRow(
        column(width = 7,
          box(width = 12,
            includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
          )
        ),
        column(width = 5,
          infoBox(width = 12,
            i18n()$t("Last Data Updates"),
            HTML(dataUpdatesTable(filter(latestData,
              country == "Switzerland", source %in% c("FOPH")),
              lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$aboutUI <- renderUI({
    fluidRow(
      box(title = i18n()$t("About"), width = 12,
        includeMarkdown("md/about.md")
      ),
      box(title = i18n()$t("Data Sources"), width = 12,
        dataTableOutput("sourcesTable")
      )
    )
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
                includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
              )
          ),
          column(width = 4,
            infoBox(width = 12,
              i18n()$t("Last Data Updates"),
              HTML(
                dataUpdatesTable(filter(latestData, country == i), lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
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
                includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
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
            latestDataComp(),
            lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
        icon = icon("exclamation-circle"),
        color = "purple"
      )
  })

  output$downloadUI <- renderUI({
    fluidPage(
      fluidRow(
        box(title = HTML(i18n()$t("Download estimates for Switzerland")), width = 12,
          downloadLink("downloadCHestimates", "Download estimates (.csv)")
        )
      ),
      fluidRow(
        box(title = HTML(i18n()$t("Download estimates for Europe")), width = 12,
            downloadLink("downloadEUestimates", "Download estimates (.csv)")
        )
      )
    )
  })

  output$dashboardBodyUI <- renderUI({
    tabList <- c("ch", "cantons", "greaterRegions", "download", "Comparison", countryList, "about")
    tabs <- lapply(
      tabList,
      function(i) {
        tabItem(tabName = str_c(str_remove(i, " ")), uiOutput(str_c(str_remove(i, " "), "UI")))
      })
    return(do.call(tabItems, tabs))
  })

  latestDataComp <- reactive({
    if (is.null(input$data_type_select)) {
      selectedDataType <- "Confirmed cases"
    } else {
      selectedDataType <- input$data_type_select
    }
    latestDataComp <- filter(latestData, data_type == selectedDataType)
    return(latestDataComp)
  })

  interventions <- read_csv(
    str_c(pathToInterventionData, "interventions.csv"),
    col_types = cols(
      name = col_character(),
      y = col_double(),
      text = col_character(),
      tooltip = col_character(),
      type = col_character(),
      date = col_date(format = ""),
      plotTextPosition = col_character())) %>%
    split(f = .$country)

  interventionsCH <- reactive({
    interventionsCH <- interventions[["Switzerland"]] %>%
      mutate(
        text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))
    return(interventionsCH)
  })

  caseDataSwitzerlandPlot <- reactive({
    caseDataSwitzerlandPlot <- rawData %>%
      filter(country == "Switzerland",
        source %in% c("FOPH"),
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    return(caseDataSwitzerlandPlot)
  })

  estimatesSwitzerland <- reactive({
    estimatesSwitzerland <- estimatesReSum %>%
      filter(country == "Switzerland") %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      group_by(data_type) %>%
      filter(
        between(date,
          left = estimatesDates[["Switzerland"]][["Switzerland"]][["start"]][[as.character(data_type[1])]],
          right = estimatesDates[["Switzerland"]][["Switzerland"]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()

    return(estimatesSwitzerland)
  })

  estimatesSwitzerlandPlot <- reactive({
    validate(
      need(input$estimation_type_select, "loading ...")
    )

    estimatesSwitzerlandPlot <- estimatesSwitzerland() %>%
      filter(
        source %in% c("FOPH"),
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      group_by(data_type) %>%
      filter(
        estimate_type == input$estimation_type_select) %>%
      ungroup()

    return(estimatesSwitzerlandPlot)
  })

  caseDataOverview <- reactive({
    caseDataOverview <- rawData %>%
      filter(data_type == input$data_type_select, region %in% countryList,
        !(country == "Switzerland" & source == "ECDC")
      ) %>%
      filter(country %in% validEstimates$country, region %in% validEstimates$region) %>%
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
      filter(country %in% validEstimates$country, region %in% validEstimates$region) %>%
      mutate(
        data_type = fct_drop(data_type)
      ) %>%
      group_by(country, data_type) %>%
      filter(
        estimate_type == input$estimation_type_select,
        between(date,
          left = estimatesDates[[country[1]]][[country[1]]][["start"]][[as.character(data_type[1])]],
          right = estimatesDates[[country[1]]][[country[1]]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()

      return(estimatesOverview)
  })

  # country raw data
  caseDataCountry <- lapply(countryList, function(i) {
    rawDataCountry <- rawData %>%
      filter(country == i, region == i) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    if (i == "Switzerland") {
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
        between(date,
          left = estimatesDates[[i]][[i]][["start"]][[as.character(data_type[1])]],
          right = estimatesDates[[i]][[i]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()
    if (i == "Switzerland") {
      estimatesCountry <- estimatesCountry %>%
        filter(source != "ECDC")
    }
    return(estimatesCountry)
  })
  names(estimatesCountry) <- str_remove(countryList, " ")

  estimatesEU <- reactive({
    estimatesEU <- bind_rows(estimatesCountry)
    return(estimatesEU)
  })

  output$CHinteractivePlot <- renderPlotly({
    validate(
      need(input$caseAverage, "loading ...")
    )

    caseDataCH <- caseDataSwitzerlandPlot() %>%
      filter(country == "Switzerland", region == "Switzerland")

    estimatesCH <- estimatesSwitzerlandPlot() %>%
      filter(country == "Switzerland", region == "Switzerland")

    latestDataCH <- filter(latestData, country == "Switzerland", region == "Switzerland",
      source %in% unique(estimatesCH$source)) %>%
      group_by(source) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      select(source, date) %>%
      distinct() %>%
      mutate(source = sapply(source, i18n()$t,  USE.NAMES = FALSE))

    plot <- rEffPlotly(
      caseDataCH,
      estimatesCH,
      interventionsCH(),
      plotColors,
      latestDataCH,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      popSizes = popSizes,
      language = input$lang,
      translator = i18n(),
      widgetID = NULL)
    return(plot)
  })

  output$cantonInteractivePlot <- renderPlotly({

    estimates <- estimatesSwitzerlandPlot() %>%
      filter(!str_detect(region, "grR"))
    caseData <- caseDataSwitzerlandPlot() %>%
      filter(region %in% estimates$region)

    cantonColors <- viridis(length(unique(caseData$region)))
    names(cantonColors) <- unique(caseData$region)
    cantonColors["Switzerland"] <- "#666666"

    latestDataCH <- latestData %>%
      filter(
        country == "Switzerland",
        region %in% unique(caseData$region),
        source %in% unique(caseData$source)) %>%
      group_by(source) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      select(source, date) %>%
      distinct() %>%
      mutate(source = sapply(source, i18n()$t,  USE.NAMES = FALSE))

    plot <- rEffPlotlyRegion(
      caseData = caseData,
      estimates = estimates,
      interventionsCH(),
      latestDataCH,
      startDate = min(caseData$date) - 1,
      endDate = max(caseData$date) + 1,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      popSizes = popSizes,
      regionColors = cantonColors,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL,
      visibilityNonFocus = "legendonly")

      return(plot)
  })

  output$greaterRegionInteractivePlot <- renderPlotly({

    rEffData <- estimatesSwitzerlandPlot() %>%
      filter(str_detect(region, "grR") | region == "Switzerland") %>%
      mutate(region = str_remove(region, "grR "))
    caseData <- caseDataSwitzerlandPlot() %>%
      mutate(region = str_remove(region, "grR ")) %>%
      filter(region %in% rEffData$region)

    popSizesGrR <- popSizes %>%
      filter(str_detect(region, "grR") | region == "Switzerland") %>%
      mutate(region = str_remove(region, "grR "))

    greaterRegionColors <- viridis(length(unique(caseData$region)))
    names(greaterRegionColors) <- unique(caseData$region)
    greaterRegionColors["Switzerland"] <- "#666666"

    latestDataCH <- latestData %>%
      filter(
        country == "Switzerland",
        region %in% unique(caseData$region),
        source %in% unique(caseData$source)) %>%
      group_by(source) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      select(source, date) %>%
      distinct() %>%
      mutate(source = sapply(source, i18n()$t,  USE.NAMES = FALSE))

    plot <- rEffPlotlyRegion(
      caseData = caseData,
      estimates = rEffData,
      interventionsCH(),
      latestDataCH,
      startDate = min(caseData$date) - 1,
      endDate = max(caseData$date) + 1,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      popSizes = popSizesGrR,
      regionColors = greaterRegionColors,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL,
      visibilityNonFocus = "legendonly")

    return(plot)
  })

  output$ComparisonPlot <- renderPlotly({

    caseData <- caseDataOverview()
    estimates <- estimatesOverview()

    focusCountry <- "Switzerland"
    countryColors <- viridis(length(countryList))
    names(countryColors) <- countryList
    countryColors[focusCountry] <- "#666666"

    latestDataComparison <- latestDataComp() %>%
      ungroup() %>%
      dplyr::select(source, date) %>%
      group_by(source) %>%
      filter(date == max(date)) %>%
      distinct()

    rEffPlotlyComparison(
      caseData = caseData,
      estimates = estimates,
      lastDataDate = latestDataComparison,
      startDate = min(estimates$date) - 14,
      focusCountry = focusCountry,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      popSizes = popSizes,
      countryColors = countryColors,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL)

  })

  # country plots
  lapply(countryList, function(i) {
    output[[str_c(str_remove(i, " "), "Plot")]] <- renderPlotly({

      if (i == "Switzerland") {
        latestDataCountry <- latestData %>%
          filter(country == i, source != "ECDC")
      } else {
        latestDataCountry <- latestData %>%
        filter(country == i)
      }

      latestDataCountry <- latestDataCountry %>%
        group_by(source) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        select(source, date) %>%
        distinct()

      caseData <- caseDataCountry[[str_remove(i, " ")]] %>%
        filter(
          data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
        mutate(
          data_type = fct_drop(data_type)
        )

      estimatesCountry <- estimatesCountry[[str_remove(i, " ")]] %>%
        filter(
          estimate_type == input$estimation_type_select,
          data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
        mutate(
          data_type = fct_drop(data_type)
        )

      interventionsCountry <- interventions[[i]] %>%
        mutate(
          text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
          tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))

      plot <- rEffPlotly(
        caseData = caseData,
        estimates = estimatesCountry,
        interventions = interventionsCountry,
        plotColors = plotColors,
        lastDataDate = latestDataCountry,
        startDate = min(estimatesCountry$date) - 14,
        fixedRangeX = fixedRangeX,
        fixedRangeY = fixedRangeY,
        logCaseYaxis = input$logCases,
        caseAverage = input$caseAverage,
        caseNormalize = input$caseNormalize,
      popSizes = popSizes,
        translator = i18n(),
        language = input$lang,
        widgetID = NULL)
      return(plot)
    })
  })

  # download
  output$downloadCHestimates <- downloadHandler(
    filename = function() {
      str_c(format(Sys.Date(), "%Y%m%d"), "-ReEstimatesCH.csv")
    },
    content = function(file) {
      write_csv(estimatesSwitzerland(), file)
    }
  )

  output$downloadEUestimates <- downloadHandler(
    filename = function() {
      str_c(format(Sys.Date(), "%Y%m%d"), "-ReEstimatesEU.csv")
    },
    content = function(file) {
      write_csv(estimatesEU(), file)
    }
  )

  # source table
  output$sourcesTable <- renderDataTable({
    tableData <- latestData %>%
      ungroup() %>%
      dplyr::select(source, sourceLong, data_type, url) %>%
      distinct() %>%
      group_by(source, sourceLong, url) %>%
      dplyr::summarize(data_type = str_c(data_type, collapse = ", ")) %>%
      mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
      dplyr::select("Source" = source, "Description" = sourceLong, "Data types" = data_type, "URL" = url) %>%
      arrange(Source)

    return(tableData)
  }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))

}
