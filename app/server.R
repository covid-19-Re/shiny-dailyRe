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
          string = list.files(path = pathToCountryData, pattern = ".*-Estimates", recursive = TRUE),
          pattern = "/(.*)-.*"
        )[, 2]
        )
    ) %>%
    left_join(distinct(dplyr::select(popData, countryIso3, country, continent)), by = "countryIso3")

  countryListContinent <- countryList %>%
    split(f = .$continent)

  # reactive Data
  interventions <- reactivePoll(1000, session,
    checkFunc = function() {
      file.mtime(str_c(pathToInterventionData, "interventions.csv"))
    },
    valueFunc = function() {
      interventions <- read_csv(
        str_c(pathToInterventionData, "interventions.csv"),
        col_types = cols(
          .default = col_character(),
          date = col_date(format = ""),
          y = col_double()
        )) %>%
        split(f = .$countryIso3)
      return(interventions)
    }
  )

  reDataEurope <- reactivePoll(1000, session,
    checkFunc = function() {
      # check estimates (only load data & deconvoluted once script is complete)
      file.mtime(
        list.files(path = file.path(pathToCountryData, "Europe"),
          full.names = TRUE, pattern = ".*-Estimates")
      )
    },
    valueFunc = function() {
      reDataEurope <- list(caseData = list(), estimates = list(), estimateRanges = list(), tests = list())
      countries <- str_match(
          string = list.files(path = file.path(pathToCountryData, "Europe"), pattern = ".*-Estimates"),
          pattern = "(.*)-.*")[, 2]

      for (icountry in countries) {
        deconvolutedData <- readRDS(file.path(pathToCountryData, "Europe", str_c(icountry, "-DeconvolutedData.rds")))
        caseData <- readRDS(file.path(pathToCountryData, "Europe", str_c(icountry, "-Data.rds")))
        estimates <- readRDS(file.path(pathToCountryData, "Europe", str_c(icountry, "-Estimates.rds")))

        if (is.null(caseData) | is.null(deconvolutedData) | is.null(estimates)) {
          # this should theoretically never happen (anymore)
          next
        }

        deconvolutedData <- deconvolutedData %>%
          dplyr::select(-variable) %>%
          mutate(data_type = str_sub(data_type, 11)) %>%
          group_by(date, region, country, source, data_type) %>%
          summarise(
            deconvoluted = mean(value),
            deconvolutedLow = deconvoluted - sd(value),
            deconvolutedHigh = deconvoluted + sd(value),
            .groups = "keep"
          )

        caseData <- caseData %>%
          pivot_wider(names_from = "variable", values_from = "value") %>%
          left_join(deconvolutedData, by = c("country", "region", "source", "data_type", "date")) %>%
          arrange(countryIso3, region, source, data_type, date)

        testsPath <- file.path(pathToCountryData, "Europe", str_c(icountry, "-Tests.rds"))
        if (file.exists(testsPath)) {
          reDataEurope$tests[[icountry]] <- readRDS(testsPath)
        }

        reDataEurope$caseData[[icountry]] <- caseData
        reDataEurope$estimates[[icountry]] <- estimates
        reDataEurope$estimateRanges[[icountry]] <- estimateRanges(
          caseData,
          minConfirmedCases = 100,
          delays = delaysDf)[[icountry]]
      }
      return(reDataEurope)
    }
  )

  reDataAfrica <- reactivePoll(1000, session,
    checkFunc = function() {
      # check estimates (only load data & deconvoluted once script is complete)
      file.mtime(
        list.files(path = file.path(pathToCountryData, "Africa"),
          full.names = TRUE, pattern = ".*-Estimates")
      )
    },
    valueFunc = function() {
      reDataAfrica <- list(caseData = list(), estimates = list(), estimateRanges = list(), tests = list())
      countries <- str_match(
          string = list.files(path = file.path(pathToCountryData, "Africa"), pattern = ".*-Estimates"),
          pattern = "(.*)-.*")[, 2]

      for (icountry in countries) {
        deconvolutedData <- readRDS(file.path(pathToCountryData, "Africa", str_c(icountry, "-DeconvolutedData.rds")))
        caseData <- readRDS(file.path(pathToCountryData, "Africa", str_c(icountry, "-Data.rds")))
        estimates <- readRDS(file.path(pathToCountryData, "Africa", str_c(icountry, "-Estimates.rds")))

        if (is.null(caseData) | is.null(deconvolutedData) | is.null(estimates)) {
          # this should theoretically never happen (anymore)
          next
        }

        deconvolutedData <- deconvolutedData %>%
          dplyr::select(-variable) %>%
          mutate(data_type = str_sub(data_type, 11)) %>%
          group_by(date, region, country, source, data_type) %>%
          summarise(
            deconvoluted = mean(value),
            deconvolutedLow = deconvoluted - sd(value),
            deconvolutedHigh = deconvoluted + sd(value),
            .groups = "keep"
          )

        caseData <- caseData %>%
          pivot_wider(names_from = "variable", values_from = "value") %>%
          left_join(deconvolutedData, by = c("country", "region", "source", "data_type", "date")) %>%
          arrange(countryIso3, region, source, data_type, date)

        testsPath <- file.path(pathToCountryData, "Africa", str_c(icountry, "-Tests.rds"))
        if (file.exists(testsPath)) {
          reDataAfrica$tests[[icountry]] <- readRDS(testsPath)
        }

        reDataAfrica$caseData[[icountry]] <- caseData
        reDataAfrica$estimates[[icountry]] <- estimates
        reDataAfrica$estimateRanges[[icountry]] <- estimateRanges(
          caseData,
          minConfirmedCases = 100,
          delays = delaysDf)[[icountry]]
      }
      return(reDataAfrica)
    }
  )

  reDataWorld <- reactive({
    reDataWorld <- list(
      caseData = c(reDataEurope()$caseData, reDataAfrica()$caseData),
      estimates = c(reDataEurope()$estimates, reDataAfrica()$estimates),
      estimateRanges = c(reDataEurope()$estimateRanges, reDataAfrica()$estimateRanges),
      tests = c(reDataEurope()$tests, reDataAfrica()$tests))
  })

  dataSources <- reactive({
    sourceInfo <- read_csv("data/dataSources.csv", col_types = cols(.default = col_character()))

    dataSources <- bind_rows(reDataWorld()) %>%
      dplyr::select(countryIso3, country, source, data_type) %>%
      filter(data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
      left_join(sourceInfo, by = "source") %>%
      group_by(source, sourceLong, url) %>%
      dplyr::summarize(
        countries = str_c(unique(country), collapse = ", "),
        data_type = str_c(unique(data_type), collapse = ", "),
        .groups = "drop_last") %>%
      mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
      dplyr::select("Source" = source, "Description" = sourceLong,
        "Countries" = countries, "Data types" = data_type, "URL" = url)

    return(dataSources)
  })

  updateData <- reactivePoll(1000, session,
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

      countryData <- loadCountryData(icountry, countryList$continent[countryList$countryIso3 == icountry])

      caseData <- countryData$caseData %>%
        filter(
          region == icountry,
          data_type %in% c("Confirmed cases", "Confirmed cases / tests",
            "Hospitalized patients", "Deaths", "Excess deaths")) %>%
        mutate(
          data_type = fct_drop(data_type)
        )

      estimatePlotRanges <- countryData$estimateRanges[[icountry]]

      estimates <- countryData$estimates %>%
        filter(
          estimate_type == input$estimation_type_select,
          region == icountry,
          data_type %in% c("Confirmed cases", "Confirmed cases / tests",
            "Hospitalized patients", "Deaths", "Excess deaths")) %>%
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

      interventionsCountry <- interventions()[[icountry]]

      if (!is.null(interventionsCountry)) {
        interventionsCountry <- mutate(interventionsCountry,
          text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
          tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))
      }

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
        showHelpBox = FALSE,
        translator = i18n(),
        language = input$lang,
        widgetID = NULL)
      return(plot)
    })
  })

  # Swiss Plots
  output$CHEcountryPlot <- renderPlotly({
    validate(
      need(input$estimation_type_select, "loading ...")
    )

    updateDataCountry <- updateData()[["CHE"]] %>%
      filter(region == "CHE")

    countryData <- loadCountryData("CHE", "Europe")

    caseData <- countryData$caseData %>%
      filter(
        region == "CHE",
        data_type %in% c("Confirmed cases", "Hospitalized patients",
          "Confirmed cases / tests", "Deaths", "Excess deaths")) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- countryData$estimateRanges[["CHE"]]

    estimates <- countryData$estimates %>%
      filter(
        estimate_type == input$estimation_type_select,
        region == "CHE",
        data_type %in% c("Confirmed cases", "Hospitalized patients",
          "Confirmed cases / tests", "Deaths", "Excess deaths")) %>%
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

    interventionsCountry <- interventions()[["CHE"]]

    if (!is.null(interventionsCountry)) {
      interventionsCountry <- mutate(interventionsCountry,
        text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))
    }

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
      showTraces = "Confirmed cases / tests",
      showTracesMode = "not",
      showHelpBox = FALSE,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL)
    return(plot)
  })

  output$CHEregionPlot <- renderPlotly({

    countryData <- loadCountryData("CHE", "Europe")

    caseData <- countryData$caseData %>%
      filter(!str_detect(region, "grR")) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- countryData$estimateRanges[["CHE"]]

    estimates <- countryData$estimates %>%
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

    interventionsCHE <- interventions()[["CHE"]] %>%
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
      interventionsCHE,
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
      regionTitle = "Canton",
      regionColors = cantonColors,
      translator = i18n(),
      language = input$lang,
      widgetID = NULL,
      focusRegion = "CHE",
      visibilityNonFocus = "legendonly")

      return(plot)
  })

  output$CHEgreaterRegionPlot <- renderPlotly({

    countryData <- loadCountryData("CHE", "Europe")

    caseData <- countryData$caseData %>%
      filter(str_detect(region, "grR") | region == "CHE") %>%
      mutate(
        region = str_remove(region, "grR "),
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- countryData$estimateRanges[["CHE"]]

    estimates <- countryData$estimates %>%
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

    interventionsCHE <- interventions()[["CHE"]] %>%
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
      interventionsCHE,
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
      focusRegion = "CHE",
      visibilityNonFocus = "legendonly")

      return(plot)
  })

  output$WorldComparisonPlot <- renderPlotly({

    reDataWorld <- reDataWorld()

    caseData <- bind_rows(reDataWorld$caseData) %>%
      filter(
        data_type == input$data_type_select_world,
        region %in% countryList$countryIso3) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- reDataWorld$estimateRanges

    estimates <- bind_rows(reDataWorld$estimates) %>%
      filter(
        data_type == input$data_type_select_world,
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
      filter(data_type == input$data_type_select_world) %>%
      ungroup() %>%
      dplyr::select(-region) %>%
      group_by(countryIso3, country, source, data_type) %>%
      dplyr::summarize(
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

  output$EuropeComparisonPlot <- renderPlotly({

    reDataEurope <- reDataEurope()

    caseData <- bind_rows(reDataEurope$caseData) %>%
      filter(
        data_type == input$data_type_select_europe,
        region %in% countryListContinent[["Europe"]]$countryIso3) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- reDataEurope$estimateRanges

    estimates <- bind_rows(reDataEurope$estimates) %>%
      filter(
        data_type == input$data_type_select_europe,
        estimate_type == input$estimation_type_select,
        region %in% countryListContinent[["Europe"]]$countryIso3) %>%
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
    countryColors1 <- viridis(length(reDataEurope$caseData))
    names(countryColors1) <- countryList$country[countryList$countryIso3 %in% names(reDataEurope$caseData)]
    countryColors1[focusCountry] <- "#666666"

    countryColors2 <- saturation(countryColors1, value = 0.1)
    names(countryColors2) <- str_c(names(countryColors1), " truncated")

    countryColors <- c(countryColors1, countryColors2)
    countryColors[str_c(focusCountry, " truncated")] <- "#BBBBBB"

    updateDataComparison <- bind_rows(updateData()) %>%
      filter(data_type == input$data_type_select_europe) %>%
      ungroup() %>%
      dplyr::select(-region) %>%
      group_by(countryIso3, country, source, data_type) %>%
      dplyr::summarize(
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

  output$AfricaComparisonPlot <- renderPlotly({

    reDataAfrica <- reDataAfrica()

    caseData <- bind_rows(reDataAfrica$caseData) %>%
      filter(
        data_type == input$data_type_select_africa,
        region %in% countryListContinent[["Africa"]]$countryIso3) %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- reDataAfrica$estimateRanges

    estimates <- bind_rows(reDataAfrica$estimates) %>%
      filter(
        data_type == input$data_type_select_africa,
        estimate_type == input$estimation_type_select,
        region %in% countryListContinent[["Africa"]]$countryIso3) %>%
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

    focusCountry <- "South Africa"
    countryColors1 <- viridis(length(reDataAfrica$caseData))
    names(countryColors1) <- countryList$country[countryList$countryIso3 %in% names(reDataAfrica$caseData)]
    countryColors1[focusCountry] <- "#666666"

    countryColors2 <- saturation(countryColors1, value = 0.1)
    names(countryColors2) <- str_c(names(countryColors1), " truncated")

    countryColors <- c(countryColors1, countryColors2)
    countryColors[str_c(focusCountry, " truncated")] <- "#BBBBBB"

    updateDataComparison <- bind_rows(updateData()) %>%
      filter(data_type == input$data_type_select_africa) %>%
      ungroup() %>%
      dplyr::select(-region) %>%
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

  output$ZAFregionPlot <- renderPlotly({

    countryData <- loadCountryData("ZAF", "Africa")

    caseData <- countryData$caseData %>%
      mutate(
        data_type = fct_drop(data_type)
      )

    estimatePlotRanges <- countryData$estimateRanges[["ZAF"]]

    estimates <- countryData$estimates %>%
      filter(
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
          left = estimatePlotRanges[["ZAF"]][["start"]][[as.character(data_type[1])]],
          right = estimatePlotRanges[["ZAF"]][["end"]][[as.character(data_type[1])]]),
      ) %>%
      ungroup()

    #filter out regions without estimations due to low case counts
    caseData <- caseData %>%
      filter(region %in% unique(estimates$region))

    interventionsZAF <- interventions()[["ZAF"]]
    if (!is.null(interventionsZAF)) {
      interventionsZAF <- mutate(interventionsZAF,
        text = sapply(text, i18n()$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, i18n()$t,  USE.NAMES = FALSE))
    }

    updateDataZAF <- updateData()[["ZAF"]] %>%
      filter(
        region %in% unique(caseData$region),
        source %in% unique(caseData$source)) %>%
      group_by(source) %>%
      filter(lastChanged == max(lastChanged)) %>%
      ungroup() %>%
      dplyr::select(source, data_type, lastChanged) %>%
      distinct() %>%
      mutate(source = sapply(source, i18n()$t,  USE.NAMES = FALSE))

    regionColors1 <- viridis(length(unique(caseData$region)))
    names(regionColors1) <- unique(caseData$region)
    regionColors1["ZAF"] <- "#666666"

    regionColors2 <- saturation(regionColors1, value = 0.1)
    names(regionColors2) <- str_c(names(regionColors1), " truncated")

    regionColors <- c(regionColors1, regionColors2)
    regionColors["ZAF truncated"] <- "#BBBBBB"

    plot <- rEffPlotlyRegion(
      caseData = caseData,
      estimates = estimates,
      interventionsZAF,
      updateDataZAF,
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
      regionColors = regionColors,
      regionTitle = "Province",
      translator = i18n(),
      language = input$lang,
      widgetID = NULL,
      focusRegion = "ZAF",
      visibilityNonFocus = "legendonly")

      return(plot)
  })

  # download
  output$downloadCHEstimates <- downloadHandler(
    filename = function() {
      str_c(format(Sys.Date(), "%Y%m%d"), "-ReEstimatesCH.csv")
    },
    content = function(file) {
      write_csv(reDataEurope()$estimates[["CHE"]], file)
    }
  )

  output$downloadAllEstimates <- downloadHandler(
    filename = function() {
      str_c(format(Sys.Date(), "%Y%m%d"), "-ReEstimates.csv")
    },
    content = function(file) {
      write_csv(bind_rows(reDataWorld()$estimates), file)
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
        menuSubItem(HTML(i18n()$t("Switzerland")), tabName = "CHEcountry", icon = icon("chart-area")),
        menuSubItem(HTML(i18n()$t("R<sub>e</sub> by canton")), tabName = "CHEregion", icon = icon("chart-area")),
        menuSubItem(HTML(i18n()$t("R<sub>e</sub> for greater Regions")),
          tabName = "CHEgreaterRegions", icon = icon("chart-area"))
      ),
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Europe")),
        expandedName = "euMenu", startExpanded = stateVals$sidebarExpanded == "euMenu",
        menuSubItem("All european countries", tabName = "EuropeComparison", icon = icon("chart-area")),
        lapply(countryListContinent$Europe$countryIso3, function(i) {
          menuSubItem(countryList$country[countryList$countryIso3 == i], tabName = i, icon = icon("chart-area"))
        })
      ),
      menuItem(HTML(i18n()$t("R<sub>e</sub> in South Africa")),
        expandedName = "ZAFmenu", startExpanded = stateVals$sidebarExpanded == "ZAFmenu",
        menuSubItem(HTML(i18n()$t("South Africa")), tabName = "ZAF", icon = icon("chart-area")),
        menuSubItem(HTML(i18n()$t("R<sub>e</sub> by province")), tabName = "ZAFregion", icon = icon("chart-area"))
      ),
      menuItem(HTML(i18n()$t("R<sub>e</sub> in Africa")),
        expandedName = "africaMenu", startExpanded = stateVals$sidebarExpanded == "africaMenu",
        menuSubItem("All african countries", tabName = "AfricaComparison", icon = icon("chart-area")),
        lapply(countryListContinent$Africa$countryIso3, function(i) {
          menuSubItem(countryList$country[countryList$countryIso3 == i], tabName = i, icon = icon("chart-area"))
        })
      ),
      menuItem(HTML(i18n()$t("All countries")), tabName = "WorldComparison", icon = icon("chart-area")),
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
          checkboxInput("caseLoess", "Show smoothed data (Loess Fit)", FALSE),
          checkboxInput("caseDeconvoluted", "Show estimated infection times (deconvolution)", FALSE),
        ),
        HTML("<i>Plot 2 - R<sub>e</sub> estimates</i>"),
        div(style = "margin-left:10px !important; margin-top:10px",
          radioButtons("estimation_type_select", "Select estimation type to show",
            choices = c("sliding window" = "Cori_slidingWindow", "step-wise constant" = "Cori_step"),
            selected = "Cori_slidingWindow", inline = FALSE)
        )
      )
  })

  # dca: unfortunately not interactive... so mostly only confusing atm
  # output$messageMenu <- renderMenu({
  #   updateDataMsg <- bind_rows(updateData()) %>%
  #     ungroup() %>%
  #     select(-region, -source, -data_type) %>%
  #     group_by(countryIso3) %>%
  #     filter(lastChanged == max(lastChanged)) %>%
  #     slice_head(n = 1)

  #   msgs <- apply(updateDataMsg[1:5, ], 1, function(row) {
  #     messageItem(
  #       from = row[["country"]],
  #       message = str_c("New data: ", row[["lastChanged"]]),
  #       icon = icon("database"))
  #   })
  #   dropdownMenu(type = "messages", .list = msgs)
  # })

  output$CHEcountryUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) in Switzerland")),
        width = 12,
        plotlyOutput("CHEcountryPlot", width = "100%", height = "800px"),
        HTML(
          str_c("<p><small>",
            i18n()$t(
              str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                "Hovering the mouse over data points shows details.")),
            "</small></p>")
        )
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
              dataUpdatesTable(updateData()$CHE,
                dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$CHEregionUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for cantons")),
      width = 12,
        plotlyOutput("CHEregionPlot", width = "100%", height = "800px"),
        HTML(
          str_c("<p><small>",
            i18n()$t(
              str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                "Hovering the mouse over data points shows details.")),
            "</small></p>")
        )
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
              dataUpdatesTable(updateData()$CHE,
                dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$CHEgreaterRegionsUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t(
          "Estimating the effective reproductive number (R<sub>e</sub>) for greater regions of Switzerland")),
        width = 12,
        plotlyOutput("CHEgreaterRegionPlot", width = "100%", height = "800px"),
        HTML(
          str_c("<p><small>",
            i18n()$t(
              str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                "Hovering the mouse over data points shows details.")),
            "</small></p>")
        )
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
              dataUpdatesTable(updateData()$CHE,
                dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$ZAFregionUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t("Estimating the effective reproductive number (R<sub>e</sub>) for provinces")),
      width = 12,
        plotlyOutput("ZAFregionPlot", width = "100%", height = "800px"),
        HTML(
          str_c("<p><small>",
            i18n()$t(
              str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                "Hovering the mouse over data points shows details.")),
            "</small></p>")
        ),
        HTML(str_c("Data provided by the ",
          "<a href=\"https://github.com/dsfsi/covid19za\">Data Science and Social Impact Research",
          "Group @ U Pretoria</a> and the <a href=\"https://www.krisp.org.za/ngs-sa/\">Network for Genomics",
          "Surveillance in South Africa (NGS-SA)</a>"))
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
              dataUpdatesTable(updateData()$ZAF,
                dateFormat = i18n()$t("%Y-%m-%d"))),
              icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  # country UIs
  lapply(countryList$countryIso3[countryList$countryIso3 != "ZAF"], function(i) {
    output[[str_c(i, "UI")]] <- renderUI({
      fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) - ",
          countryList$country[countryList$countryIso3 == i]))),
          width = 12,
          plotlyOutput(str_c(i, "Plot"), width = "100%", height = "800px"),
          HTML(
            str_c("<p><small>",
              i18n()$t(
                str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                  "Hovering the mouse over data points shows details.")),
              "</small></p>")
          )
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
                dataUpdatesTable(updateData()[[i]], dateFormat = i18n()$t("%Y-%m-%d"))
              ),
              icon = icon("exclamation-circle"),
              color = "purple"
            )
          )
        )
      )
    })
  })

  output$ZAFUI <- renderUI({
    fluidRow(
      box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) - ",
        countryList$country[countryList$countryIso3 == "ZAF"]))),
        width = 12,
        plotlyOutput("ZAFPlot", width = "100%", height = "800px"),
        HTML(
          str_c("<p><small>",
            i18n()$t(
              str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                "Hovering the mouse over data points shows details.")),
            "</small></p>")
        ),
        HTML(str_c("Data provided by the ",
          "<a href=\"https://github.com/dsfsi/covid19za\">Data Science and Social Impact Research",
          "Group @ U Pretoria</a> and the <a href=\"https://www.krisp.org.za/ngs-sa/\">Network for Genomics",
          "Surveillance in South Africa (NGS-SA)</a>"))
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
              dataUpdatesTable(updateData()[["ZAF"]], dateFormat = i18n()$t("%Y-%m-%d"))
            ),
            icon = icon("exclamation-circle"),
            color = "purple"
          )
        )
      )
    )
  })

  output$EuropeComparisonUI <- renderUI({
    fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) in Europe - ",
          "Comparison"))),
          width = 12,
          radioButtons("data_type_select_europe", "Select Data Type to compare",
            choices = c("Confirmed cases", "Hospitalized patients", "Deaths"),
            selected = "Confirmed cases", inline = TRUE),
          plotlyOutput("EuropeComparisonPlot", width = "100%", height = "800px"),
          HTML(
            str_c("<p><small>",
              i18n()$t(
                str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                  "Hovering the mouse over data points shows details.")),
              "</small></p>")
          )
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
              )
          ),
          column(width = 4,
            uiOutput("EuropeComparisonDataSourceUI")
          )
        )
      )
  })

  output$EuropeComparisonDataSourceUI <- renderUI({
    infoBox(width = 12,
        i18n()$t("Last Data Updates"),
        HTML(
          dataUpdatesTable(
            updateData()[countryListContinent$Europe$countryIso3],
            dateFormat = i18n()$t("%Y-%m-%d"))),
        icon = icon("exclamation-circle"),
        color = "purple"
      )
  })

  output$AfricaComparisonUI <- renderUI({
    fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) in Africa - ",
          "Comparison"))),
          width = 12,
          radioButtons("data_type_select_africa", "Select Data Type to compare",
            choices = c("Confirmed cases", "Hospitalized patients", "Deaths"),
            selected = "Confirmed cases", inline = TRUE),
          plotlyOutput("AfricaComparisonPlot", width = "100%", height = "800px"),
          HTML(
            str_c("<p><small>",
              i18n()$t(
                str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                  "Hovering the mouse over data points shows details.")),
              "</small></p>")
          )
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
              )
          ),
          column(width = 4,
            uiOutput("AfricaComparisonDataSourceUI")
          )
        )
      )
  })

  output$AfricaComparisonDataSourceUI <- renderUI({
    infoBox(width = 12,
        i18n()$t("Last Data Updates"),
        HTML(
          dataUpdatesTable(
            updateData()[countryListContinent$Africa$countryIso3],
            dateFormat = i18n()$t("%Y-%m-%d"))),
        icon = icon("exclamation-circle"),
        color = "purple"
      )
  })

  output$WorldComparisonUI <- renderUI({
    fluidRow(
        box(title = HTML(i18n()$t(str_c("Estimating the effective reproductive number (R<sub>e</sub>) - ",
          "all included countries"))),
          width = 12,
          radioButtons("data_type_select_world", "Select Data Type to compare",
            choices = c("Confirmed cases", "Hospitalized patients", "Deaths"),
            selected = "Confirmed cases", inline = TRUE),
          plotlyOutput("WorldComparisonPlot", width = "100%", height = "800px"),
          HTML(
            str_c("<p><small>",
              i18n()$t(
                str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                  "Hovering the mouse over data points shows details.")),
              "</small></p>")
          )
        ),
        fluidRow(
          column(width = 8,
              box(width = 12,
                includeMarkdown(str_c("md/methodsOnly_", input$lang, ".md"))
              )
          ),
          column(width = 4,
            uiOutput("WorldComparisonDataSourceUI")
          )
        )
      )
  })

  output$WorldComparisonDataSourceUI <- renderUI({
    infoBox(width = 12,
        i18n()$t("Last Data Updates"),
        HTML(
          dataUpdatesTable(
            updateData()[countryList$countryIso3],
            dateFormat = i18n()$t("%Y-%m-%d"))),
        icon = icon("exclamation-circle"),
        color = "purple"
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
    tabList <- c(
      "CHEcountry", "CHEregion", "CHEgreaterRegions",
      "EuropeComparison",
      "WorldComparison",
      "AfricaComparison",
      countryList$countryIso3,
      "ZAFregion",
      "download", "about")
    tabs <- lapply(
      tabList,
      function(i) {
        tabItem(tabName = i, uiOutput(str_c(i, "UI")))
      })
    return(do.call(tabItems, tabs))
  })

}
