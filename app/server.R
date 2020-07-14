server <- function(input, output, session) {

  stateVals <- reactiveValues(lang = "en-gb", tabs = "ch", sidebarExpanded = "chMenu")

  # record state on language change
  observeEvent(input$lang, {
      stateVals$lang <- input$lang
      stateVals$tabs <- input$tabs
      stateVals$sidebarExpanded <- input$sidebarItemExpanded
  })

  # translation
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    # restore selected tabs
    updateTabItems(session, "tabs", selected = stateVals$tabs)
    return(translator)
  })

  # static Data
  popData <- readRDS(pathToPopData)

  countryList <- tibble(
    countryIso3 = unique(
      str_match(
        string = list.files(path = pathToCountryData, pattern = ".*-Estimates"),
        pattern = "(.*)-.*"
      )[, 2]
  )) %>%
  left_join(distinct(select(popData, countryIso3, country, continent)), by = "countryIso3")

  interventions <- read_csv(
    str_c(pathToInterventionData, "interventions.csv"),
    col_types = cols(
      .default = col_character(),
      date = col_date(format = ""),
      y = col_double()
    )) %>%
    split(f = .$countryIso3)

  # reactive Data
  reData <- reactivePoll(10 * 60 * 1000, session,
    checkFunc = function() {
      # check estimates (only load data & deconvoluted once script is complete)
      file.mtime(list.files(path = pathToCountryData, full.names = TRUE, pattern = ".*-Estimates"))
    },
    valueFunc = function() {
      reData <- list(caseData = list(), estimates = list(), estimateRanges = list())
      for (icountry in countryList$countryIso3) {
        deconvolutedData <- readRDS(file.path(pathToCountryData, str_c(icountry, "-DeconvolutedData.rds"))) %>%
          select(-variable) %>%
          mutate(data_type = str_sub(data_type, 11)) %>%
          group_by(date, region, country, source, data_type) %>%
          summarise(
            deconvoluted = mean(value),
            deconvolutedLow = deconvoluted - sd(value),
            deconvolutedHigh = deconvoluted + sd(value),
            .groups = "keep"
          )

        caseData <- readRDS(file.path(pathToCountryData, str_c(icountry, "-Data.rds"))) %>%
          pivot_wider(names_from = "variable", values_from = "value") %>%
          left_join(deconvolutedData, by = c("country", "region", "source", "data_type", "date")) %>%
          arrange(countryIso3, region, source, data_type, date)

        estimates <- readRDS(file.path(pathToCountryData, str_c(icountry, "-Estimates.rds")))

        reData$caseData[[icountry]] <- caseData
        reData$estimates[[icountry]] <- estimates
        reData$estimateRanges[[icountry]] <- estimateRanges(
          caseData,
          minConfirmedCases = 100,
          delays = delaysDf)[[icountry]]
      }
      return(reData)
    }
  )

  dataSources <- reactive({
    sourceInfo <- read_csv("data/dataSources.csv", col_types = cols(.default = col_character()))

    dataSources <- bind_rows(reData()$caseData) %>%
      select(countryIso3, country, source, data_type) %>%
      filter(data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
      left_join(sourceInfo, by = "source") %>%
      group_by(source, sourceLong, url) %>%
      summarize(
        countries = str_c(unique(country), collapse = ", "),
        data_type = str_c(unique(data_type), collapse = ", "),
        .groups = "drop_last") %>% 
      mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
      select("Source" = source, "Description" = sourceLong,
        "Countries" = countries, "Data types" = data_type, "URL" = url)

    return(dataSources)
  })  

  updateData <- reactivePoll(10 * 60 * 1000, session,
    checkFunc = function() {
      file.mtime(pathToUpdataData)
    },
    valueFunc = function() {
      updateData <- readRDS(pathToUpdataData)
      return(updateData)
    }
  )

  # country plots
  lapply(countryList$countryIso3, function(icountry) {
    output[[str_c(icountry, "Plot")]] <- renderPlotly({
      validate(
        need(input$estimation_type_select, "loading ...")
      )

      updateDataCountry <- updateData()[[icountry]] %>%
        filter(region == icountry)

      reData <- reData()

      caseData <- reData$caseData[[icountry]] %>%
        filter(
          region == icountry,
          data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
        mutate(
          data_type = fct_drop(data_type)
        )

      estimatePlotRanges <- reData$estimateRanges[[icountry]]

      estimates <- reData$estimates[[icountry]] %>%
        filter(
          estimate_type == input$estimation_type_select,
          region == icountry,
          data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
        mutate(
          region = fct_drop(region),
          country = fct_drop(country),
          data_type = fct_drop(data_type)
        ) %>%
        group_by(data_type) %>%
        filter(
          between(date,
            left = estimatePlotRanges[[icountry]][["start"]][[as.character(data_type[1])]],
            right = estimatePlotRanges[[icountry]][["end"]][[as.character(data_type[1])]]),
        ) %>%
        ungroup()

      interventionsCountry <- interventions[[icountry]] %>%
        mutate(
          text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
          tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))

      plot <- rEffPlotly(
        caseData = caseData,
        estimates = estimates,
        interventions = interventionsCountry,
        plotColors = plotColors,
        lastDataDate = updateDataCountry,
        startDate = min(estimates$date) - 14,
        fixedRangeX = fixedRangeX,
        fixedRangeY = fixedRangeY,
        logCaseYaxis = input$logCases,
        caseAverage = input$caseAverage,
        caseNormalize = input$caseNormalize,
        caseLoess = input$caseLoess,
        caseDeconvoluted = input$caseDeconvoluted,
        translator = i18n(),
        language = input$lang,
        widgetID = NULL)
      return(plot)
    })
  })

  output$cantonPlot <- renderPlotly({

    reData <- reData()

    caseData <- reData$caseData[["CHE"]] %>%
      filter(!str_detect(region, "grR")) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- reData$estimateRanges[[icountry]]

    estimates <- reData$estimates[["CHE"]] %>%
      filter(
        !str_detect(region, "grR"),
        estimate_type == input$estimation_type_select,
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
      mutate(
        region = fct_drop(region),
        country = fct_drop(country),
        data_type = fct_drop(data_type)
      ) %>%
      group_by(data_type) %>%
      filter(
        between(date,
          left = estimatePlotRanges[["CHE"]][["start"]][[as.character(data_type[1])]],
          right = estimatePlotRanges[["CHE"]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()

    #filter out regions without estimations due to low case counts
    caseData <- caseData %>%
      filter(region %in% unique(estimates$region))

    interventionsCH <- interventions[["CHE"]] %>%
      mutate(
        text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))

    updateDataCHE <- updateData()[["CHE"]] %>%
      filter(
        region %in% unique(caseData$region),
        source %in% unique(caseData$source)) %>%
      group_by(source) %>%
      filter(lastChanged == max(lastChanged)) %>%
      ungroup() %>%
      dplyr::select(source, data_type, lastChanged) %>%
      distinct() %>%
      mutate(source = sapply(source, i18n()$t,  USE.NAMES = FALSE))

    cantonColors1 <- viridis(length(unique(caseData$region)))
    names(cantonColors1) <- unique(caseData$region)
    cantonColors1["CHE"] <- "#666666"

    cantonColors2 <- saturation(cantonColors1, value = 0.1)
    names(cantonColors2) <- str_c(names(cantonColors1), " truncated")

    cantonColors <- c(cantonColors1, cantonColors2)
    cantonColors["CHE truncated"] <- "#BBBBBB"

    plot <- rEffPlotlyRegion(
      caseData = caseData,
      estimates = estimates,
      interventionsCH,
      updateDataCHE,
      startDate = min(caseData$date) - 1,
      endDate = max(caseData$date) + 1,
      caseDataRightTruncation = 2,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      caseLoess = input$caseLoess,
      caseDeconvoluted = input$caseDeconvoluted,
      regionColors = cantonColors,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL,
      focusRegion = "Switzerland (Total)",
      visibilityNonFocus = "legendonly")

      return(plot)
  })

  output$greaterRegionPlot <- renderPlotly({

    reData <- reData()

    caseData <- reData$caseData[["CHE"]] %>%
      filter(str_detect(region, "grR") | region == "CHE") %>%
      mutate(
        region = str_remove(region, "grR "),
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- reData$estimateRanges[[icountry]]

    estimates <- reData$estimates[["CHE"]] %>%
      filter(
        str_detect(region, "grR") | region == "CHE",
        estimate_type == input$estimation_type_select,
        data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
      mutate(
        region = fct_drop(str_remove(region, "grR ")),
        country = fct_drop(country),
        data_type = fct_drop(data_type)
      ) %>%
      group_by(data_type) %>%
      filter(
        between(date,
          left = estimatePlotRanges[["CHE"]][["start"]][[as.character(data_type[1])]],
          right = estimatePlotRanges[["CHE"]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()

    #filter out regions without estimations due to low case counts
    caseData <- caseData %>%
      filter(region %in% unique(estimates$region))

    interventionsCH <- interventions[["CHE"]] %>%
      mutate(
        text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))

    updateDataCHE <- updateData()[["CHE"]] %>%
      filter(
        region %in% unique(caseData$region),
        source %in% unique(caseData$source)) %>%
      group_by(source) %>%
      filter(lastChanged == max(lastChanged)) %>%
      ungroup() %>%
      dplyr::select(source, data_type, lastChanged) %>%
      distinct() %>%
      mutate(source = sapply(source, i18n()$t,  USE.NAMES = FALSE))

    greaterRegionsColors1 <- viridis(length(unique(caseData$region)))
    names(greaterRegionsColors1) <- unique(caseData$region)
    greaterRegionsColors1["CHE"] <- "#666666"

    greaterRegionsColors2 <- saturation(greaterRegionsColors1, value = 0.1)
    names(greaterRegionsColors2) <- str_c(names(greaterRegionsColors1), " truncated")

    greaterRegionsColors <- c(greaterRegionsColors1, greaterRegionsColors2)
    greaterRegionsColors["CHE truncated"] <- "#BBBBBB"

    plot <- rEffPlotlyRegion(
      caseData = caseData,
      estimates = estimates,
      interventionsCH,
      updateDataCHE,
      startDate = min(caseData$date) - 1,
      endDate = max(caseData$date) + 1,
      caseDataRightTruncation = 2,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      caseLoess = input$caseLoess,
      caseDeconvoluted = input$caseDeconvoluted,
      regionColors = greaterRegionsColors,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL,
      focusRegion = "Switzerland (Total)",
      visibilityNonFocus = "legendonly")

      return(plot)
  })

  output$ComparisonPlot <- renderPlotly({

    reData <- reData()

    caseData <- bind_rows(reData$caseData) %>%
      filter(
        data_type == input$data_type_select,
        region %in% countryList$countryIso3) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- reData$estimateRanges

    estimates <- bind_rows(reData$estimates) %>%
      filter(
        data_type == input$data_type_select,
        estimate_type == input$estimation_type_select,
        region %in% countryList$countryIso3) %>%
      group_by(countryIso3, data_type) %>%
      filter(
        between(date,
          left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
          right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup() %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    #filter out regions without estimations due to low case counts
    caseData <- caseData %>%
      filter(region %in% unique(estimates$region))

    focusCountry <- "Switzerland"
    countryColors1 <- viridis(length(countryList$country))
    names(countryColors1) <- countryList$country
    countryColors1[focusCountry] <- "#666666"

    countryColors2 <- saturation(countryColors1, value = 0.1)
    names(countryColors2) <- str_c(names(countryColors1), " truncated")

    countryColors <- c(countryColors1, countryColors2)
    countryColors[str_c(focusCountry, " truncated")] <- "#BBBBBB"

    updateDataComparison <- bind_rows(updateData()) %>%
      filter(data_type == input$data_type_select) %>%
      ungroup() %>%
      select(-region) %>%
      group_by(countryIso3, country, source, data_type) %>%
      summarize(
        lastChanged = max(lastChanged),
        .groups = "keep") %>%
      ungroup()

    rEffPlotlyComparison(
      caseData = caseData,
      estimates = estimates,
      lastDataDate = updateDataComparison,
      startDate = min(estimates$date) - 14,
      focusCountry = focusCountry,
      fixedRangeX = fixedRangeX,
      fixedRangeY = fixedRangeY,
      caseDataRightTruncation = 2,
      logCaseYaxis = input$logCases,
      caseAverage = input$caseAverage,
      caseNormalize = input$caseNormalize,
      caseLoess = input$caseLoess,
      caseDeconvoluted = input$caseDeconvoluted,
      countryColors = countryColors,
      translator = translator,
      language = input$lang,
      widgetID = NULL)

  })

  # download
  output$downloadCHEstimates <- downloadHandler(
    filename = function() {
      str_c(format(Sys.Date(), "%Y%m%d"), "-ReEstimatesCH.csv")
    },
    content = function(file) {
      write_csv(reData()$estimates[["CHE"]], file)
    }
  )

  output$downloadAllEstimates <- downloadHandler(
    filename = function() {
      str_c(format(Sys.Date(), "%Y%m%d"), "-ReEstimates.csv")
    },
    content = function(file) {
      write_csv(bind_rows(reData()$estimates), file)
    }
  )

  # source table
  output$sourcesTable <- renderDataTable({
    dataSources()
  }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))

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
        menuSubItem("Comparison", tabName = "Comparison", icon = icon("chart-area")),
        lapply(countryList$countryIso3, function(i) {
          menuSubItem(countryList$country[countryList$countryIso3 == i], tabName = i, icon = icon("chart-area"))
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
            selected = 1, inline = FALSE),
          HTML("<i>Diagnostics:</i>"),
          checkboxInput("caseLoess", "Show Loess fit", FALSE),
          checkboxInput("caseDeconvoluted", "Show deconvoluted case data", FALSE),
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
        plotlyOutput("CHEPlot", width = "100%", height = "800px")
      ),
      fluidRow(
        column(width = 8,
          box(width = 12,
            includeMarkdown(str_c("md/methodsCH_", input$lang, ".md"))
            )
        ),
        column(width = 4#,
          # infoBox(width = 12,
          #   i18n()$t("Last Data Updates"),
          #   HTML(
          #     dataUpdatesTable(filter(latestData,
          #       country == "Switzerland", source %in% c("FOPH")),
          #       lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
          #     icon = icon("exclamation-circle"),
          #   color = "purple"
          # )
        )
      )
    )
  })

  output$cantonsUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for cantons")),
      width = 12,
          plotlyOutput("cantonPlot", width = "100%", height = "800px")
      ),
      fluidRow(
        box(width = 12,
          includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
          )
      )
    )
  })

  output$greaterRegionsUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for greater regions of Switzerland")),
        width = 12,
        plotlyOutput("greaterRegionPlot", width = "100%", height = "800px")
      ),
      fluidRow(
        column(width = 7,
          box(width = 12,
            includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
          )
        ),
        column(width = 5
          # infoBox(width = 12,
          #   i18n()$t("Last Data Updates"),
          #   HTML(dataUpdatesTable(filter(latestData,
          #     country == "Switzerland", source %in% c("FOPH")),
          #     lastCheck, dateFormat = i18n()$t("%Y-%m-%d"))),
          #     icon = icon("exclamation-circle"),
          #   color = "purple"
          # )
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
  lapply(countryList$countryIso3, function(i) {
    output[[str_c(i, "UI")]] <- renderUI({
      fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) in Europe - ",
          countryList$country[countryList$countryIso3 == i]))),
          width = 12,
          plotlyOutput(str_c(i, "Plot"), width = "100%", height = "700px")
        ),
        fluidRow(
          box(width = 12,
            includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
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
            choices = c("Confirmed cases", "Hospitalized patients", "Deaths"),
            selected = "Confirmed cases", inline = TRUE),
          plotlyOutput("ComparisonPlot", width = "100%", height = "700px")
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
              )
          ),
          column(width = 4#,
            #uiOutput("ComparisonDataSourceUI")
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
          downloadLink("downloadCHEstimates", "Download estimates (.csv)")
        )
      ),
      fluidRow(
        box(title = HTML(i18n()$t("Download all estimates")), width = 12,
            downloadLink("downloadAllEstimates", "Download estimates (.csv)")
        )
      )
    )
  })

  output$dashboardBodyUI <- renderUI({
    tabList <- c("ch", "cantons", "greaterRegions", "download", "Comparison", countryList$countryIso3, "about")
    tabs <- lapply(
      tabList,
      function(i) {
        tabItem(tabName = str_c(i), uiOutput(str_c(i, "UI")))
      })
    return(do.call(tabItems, tabs))
  })

}
