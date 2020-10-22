server <- function(input, output, session) {

# language switching
  stateVals <- reactiveValues(
    lang = "en-gb",
    tabs = "plots",
    plotSize = "large",
    mapViewCenter = list(lng = 24.78515, lat =  33.72436),
    mapViewZoom = 2,
    regionCountrySelect = NULL)

  # record state on language change
  observeEvent(input$lang, {
      stateVals$lang <- input$lang
      stateVals$tabs <- input$tabs
      if (!is.null(input$mapPlot_center)) {
        stateVals$mapViewCenter <- input$mapPlot_center
        stateVals$mapViewZoom <- input$mapPlot_zoom
        stateVals$mapViewGroups <- input$mapPlot_groups
        stateVals$regionCountrySelect <- input$regionCountrySelect
      }
  })

  # translation
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    # restore selected tabs
    updateTabItems(session, "tabs", selected = stateVals$tabs)
    if (stateVals$tabs == "mapPlot") {
      mapPlot <- leafletProxy("mapPlot")
      mapPlot %>%
        setView(
          lng = stateVals$mapViewCenter$lng, lat = stateVals$mapViewCenter$lat,
          zoom = stateVals$mapViewZoom)
    }
    return(translator)
  })

# main ui
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem(HTML(i18n()$t("Timeseries")), tabName = "plots", icon = icon("chart-area")),
      uiOutput("timeSeriesPlotOptionsUI"),
      menuItem(i18n()$t("Maps"), tabName = "mapPlot", icon = icon("map")),
      menuItem(i18n()$t("Options"), startExpanded = TRUE,
        uiOutput("plotOptionsUI")
      ),
      menuItem(i18n()$t("About"), tabName = "about", icon = icon("question-circle")),
      selectInput("lang", i18n()$t("Language"),
        languageSelect, selected = input$lang, multiple = FALSE,
        selectize = TRUE, width = NULL, size = NULL)
    )
  })

# time series plots
  # data
    allData <- reactive({
      allData <- readRDS(pathToAllCountryData)
      return(allData)
    })

    # debounce countrySelect
    countrySelectValue <- reactive({
      input$countrySelect
    }) %>% debounce(500)

    countryData <- reactive({
      validate(need(countrySelectValue() != "", "Please select a country"))
      allData <- allData()
      countrySelectValue <- countrySelectValue()

      countryData <- list(
        caseData = filter(allData$caseData, countryIso3 %in% countrySelectValue),
        estimates = filter(allData$estimates, countryIso3 %in% countrySelectValue)
      )

      return(countryData)
    })

    updateData <- reactive({
      updateDataRaw <- readRDS(pathToUpdataData)

      updateData <- bind_rows(updateDataRaw[countrySelectValue()]) %>%
        ungroup() %>%
        dplyr::select(-country) %>%
        left_join(dplyr::select(continents, countryIso3, country), by = "countryIso3")
      return(updateData)
    })

    interventions <- reactive({
      interventions <- read_csv(
        str_c(pathToInterventionData, "interventions.csv"),
        col_types = cols(
          .default = col_character(),
          date = col_date(format = ""),
          y = col_double()
        )) %>%
        split(f = .$countryIso3)
      return(interventions)
    })

    # change plotSize based on window dimension
    # done via an observer to prevent redrawing on dimension change (rather than only when we want the
    # dimension to change)
    observe({
      if (!is.null(input$dimension)) {
        if (input$dimension[1] <= 1024) {
          stateVals$plotSize <- "small"
        } else {
          stateVals$plotSize <- "large"
        }
      }
    })

  # outputs
    output$rePlot_data_type <- renderPlotly({
      countryData <- countryData()
      updateData <- updateData()
      interventions <- interventions()
      plotSize <- stateVals$plotSize

      rEffPlotlyShiny(countryData, updateData, interventions, "data_type", input, i18n(), plotSize)
    })

    output$rePlot_region <- renderPlotly({
      countryData <- countryData()
      updateData <- updateData()
      interventions <- interventions()
      plotSize <- stateVals$plotSize

      rEffPlotlyShiny(countryData, updateData, interventions, "region", input, i18n(), plotSize)
    })

    output$rePlot_greaterRegion <- renderPlotly({
      countryData <- countryData()
      updateData <- updateData()
      interventions <- interventions()
      plotSize <- stateVals$plotSize

      rEffPlotlyShiny(countryData, updateData, interventions, "greaterRegion", input, i18n(), plotSize)
    })

  # ui
    output$timeSeriesPlotOptionsUI <- renderUI({
      validate(need(input$tabs, ""))
      if (input$tabs == "plots") {
        ui <- tagList(
          selectizeInput("countrySelect", i18n()$t("Countries"),
            countryList,
            selected = "CHE", multiple = TRUE, width = "100%", size = NULL,
            options = list(plugins = list("remove_button"),
              closeAfterSelect = TRUE, hideSelected = TRUE, sortField = "label")
          ),
          uiOutput("quickselectUI"),
          conditionalPanel(
          condition = "input.plotTabs != 'data_type' | input.countrySelect.length > 1",
            radioButtons("dataTypeSelect", i18n()$t("Select data type for comparison"),
              choices = i18n()$t("Confirmed cases"),
              selected = "Confirmed cases", inline = FALSE)
          )
        )
      } else {
        ui <- NULL
      }
      return(ui)
    })

    dataTypeChoices <- reactive({
      validate(need(!is.null(input$plotTabs), ""))

      if (input$plotTabs != "data_type") {
        splitBy <- "region"
      } else {
        splitBy <- "countryIso3"
      }

      if (dim(countryData()$estimates)[1] > 0) {
        dataTypes <- countryData()$estimates %>%
          group_by(.data[[splitBy]]) %>%
          group_split() %>%
          lapply(function(x) {
            unique(x$data_type)
          })
        dataTypeChoices <- dataTypes[[which.min(lengths(dataTypes))]]
        names(dataTypeChoices) <- sapply(dataTypeChoices, i18n()$t,  USE.NAMES = FALSE)
      } else {
        dataTypeChoices <- "None"
        names(dataTypeChoices) <- sapply(dataTypeChoices, i18n()$t,  USE.NAMES = FALSE)
      }

      return(dataTypeChoices)
    })

    observeEvent(countrySelectValue(), {
      updateRadioButtons(session, "dataTypeSelect", choices = dataTypeChoices())
    })

    observeEvent(input$plotTabs, {
      updateRadioButtons(session, "dataTypeSelect", choices = dataTypeChoices())
    })

    output$quickselectUI <- renderUI({
      ui <- tagList(
        HTML(str_c("<div class='form-group shiny-input-container' style='width: 100%;'>
        <label class='control-label' for='quickselectButtons'>",
          i18n()$t("Quick select"), ":</label>")),
        div(
          div(class = "quickSelect", actionLink("africaSelect", i18n()$t("Africa"))),
          div(class = "quickSelect", actionLink("asiaSelect", i18n()$t("Asia"))),
          div(class = "quickSelect", actionLink("europeSelect", i18n()$t("Europe"))),
          div(class = "quickSelect", actionLink("northAmericaSelect", i18n()$t("North America"))),
          div(class = "quickSelect", actionLink("oceaniaSelect", i18n()$t("Oceania"))),
          div(class = "quickSelect", actionLink("southAmericaSelect", i18n()$t("South America"))),
          div(class = "quickSelect", actionLink("clearSelect", HTML("", i18n()$t("Clear all")),
            class = "fa fa-minus-circle"))
        )
      )
      return(ui)
    })

    observeEvent(input$africaSelect, {
      updateSelectizeInput(session, "countrySelect", selected = countryList$Africa)
    })
    observeEvent(input$asiaSelect, {
      updateSelectizeInput(session, "countrySelect", selected = countryList$Asia)
    })
    observeEvent(input$europeSelect, {
      updateSelectizeInput(session, "countrySelect", selected = countryList$Europe)
    })
    observeEvent(input$northAmericaSelect, {
      updateSelectizeInput(session, "countrySelect", selected = countryList[["North America"]])
    })
    observeEvent(input$oceaniaSelect, {
      updateSelectizeInput(session, "countrySelect", selected = countryList$Oceania)
    })
    observeEvent(input$southAmericaSelect, {
      updateSelectizeInput(session, "countrySelect", selected = countryList[["South America"]])
    })
    observeEvent(input$clearSelect, {
      updateSelectizeInput(session, "countrySelect", selected = "")
    })

    output$timeseriesPlotTabsUI <- renderUI({
      
      if (length(countrySelectValue()) == 1) {
        if (countrySelectValue() == "CHE") {
          tabList <- list(
            c(name = "data_type", title = i18n()$t("Switzerland")),
            c(name = "region", title = i18n()$t("Canton")),
            c(name = "greaterRegion", title = i18n()$t("Greater regions")))
        } else if (countrySelectValue() == "ZAF") {
          tabList <- list(
            c(name = "data_type", title = i18n()$t("South Africa")),
            c(name = "region", title = i18n()$t("Province")))
        } else {
          tabList <- list(
            c(name = "data_type", title = i18n()$t(popData$country[popData$countryIso3 == countrySelectValue()])))
        }
      } else if (length(countrySelectValue()) == 0) {
          tabList <- list(
            c(name = "data_type", title = i18n()$t("Country?")))
      } else {
        tabList <- list(
          c(name = "data_type", title = i18n()$t("Country Comparison")))
      }

      tabs <- lapply(
        tabList,
        function(i) {
          uiContent <- tagList(
            plotlyOutput(str_c("rePlot_", i[[1]]), width = "100%", height = "800px") %>% withSpinner(),
            HTML(
              str_c(
                "<p><small>",
                i18n()$t(
                  str_c("<b>Interactive plot:</b> Click on legend toggles datatypes; doubleclick isolates datatypes. ",
                    "Hovering the mouse over data points shows details.")),
                "</small></p>")
            )
          )

          ui <- tabPanel(
            title = i[[2]],
            value = i[[1]],
            tagList(uiContent)
          )
          return(ui)
        })

      tabs$width <- 12
      tabs$title <- tagList(shiny::icon("chart-area"),
        HTML(i18n()$t("Estimation of the effective reproductive number (R<sub>e</sub>)")))
      tabs$id <- "plotTabs"
      return(do.call(tabBox, tabs))
    })

    output$timeseriesPlotUI <- renderUI({
      fluidRow(uiOutput("timeseriesPlotTabsUI"))
    })

    currentRestimate <- reactive({
      countryData <- countryData()
      countrySelectValue <- countrySelectValue()

      currentRestimate <- countryData$estimates %>%
        filter(
          region %in% countrySelectValue,
          data_type == "Confirmed cases",
          estimate_type == "Cori_slidingWindow") %>%
        group_by(country, region) %>%
        filter(date == max(date)) %>%
        transmute(
          country, region,
          mean = median_R_mean,
          low = median_R_lowHPD,
          high = median_R_highHPD,
          date = format(date, i18n()$t("%b-%d")),
          regionSort = factor(region, levels = countrySelectValue)
        ) %>%
        arrange(regionSort) %>%
        distinct()

      return(currentRestimate)
    })

    output$currentR <- renderUI({
      currentRestimate <- currentRestimate()

      text <- i18n()$t("Most recent effective reproductive number estimate")
      icon <- NULL
      popoverId <- "currentR"
      popoverTitle <- "Effective reproductive number estimate"
      popoverText <- str_c("This is the most recent possible estimate based on confirmed cases. ",
        "This point estimate is not consolidated yet and could change as more data comes in.")

      ui <- rValueBox(currentRestimate, text, icon, popoverId, popoverTitle, popoverText, background = "bg-light-blue")
      return(ui)
    })

    avgRestimate <- reactive({
      countryData <- countryData()
      countrySelectValue <- countrySelectValue()

      avgRestimate <- countryData$estimates %>%
        filter(
          region %in% countrySelectValue,
          data_type == "Confirmed cases",
          estimate_type == "Cori_slidingWindow") %>%
        group_by(country, region) %>%
        filter(date >= max(date) - 7) %>%
        summarize(
          mean = mean(median_R_mean),
          low = min(median_R_lowHPD),
          high = max(median_R_highHPD),
          date = str_c(format(min(date), translator$t("%b-%d")), " - ", format(max(date), translator$t("%b-%d"))),
          .groups = "drop"
        ) %>%
        mutate(
          regionSort = factor(region, levels = countrySelectValue)
        ) %>%
        arrange(regionSort)

      return(avgRestimate)
    })

    output$avgR <- renderUI({
      avgRestimate <- avgRestimate()

      text <- i18n()$t("Average effective reproductive number over the last 7 days")
      icon <- NULL
      popoverId <- "avgR"
      popoverTitle <- "Average effective reproductive number"
      popoverText <- str_c("This estimate is based on confirmed cases and averaged over the last 7 days.",
        " It is a stabilized picture of the recent epidemic dynamic.")

      ui <- rValueBox(avgRestimate, text, icon, popoverId, popoverTitle, popoverText)

      return(ui)
    })

    output$dataSourceUI <- renderUI({
      validate(need(countrySelectValue(), ""))
      infoBox(width = 3,
          i18n()$t("Last Data Updates"),
          HTML(
            dataUpdatesTable(
              updateData(),
              dateFormat = i18n()$t("%Y-%m-%d"))),
          icon = icon("exclamation-circle"),
          color = "purple"
        )
    })

    output$methodsUI <- renderUI({
      validate(need(countrySelectValue(), ""))
      methodsFileName <- "md/methodsOnly_"
      if (length(countrySelectValue()) == 1) {
        if (countrySelectValue() == "CHE") {
          methodsFileName <- "md/methodsCH_"
        }
      }

      ui <- box(width = 9, includeMarkdown(str_c(methodsFileName, input$lang, ".md")))
      return(ui)
    })

# plot options UI (timeseries & maps)

  # countries with regional data
  regionCountries <- reactive({
    allData <- allData()

    regionCountriesDf <- allData$caseData %>%
      group_by(countryIso3) %>%
      filter(length(unique(region)) > 1) %>%
      dplyr::select(countryIso3, country) %>%
      distinct()

    regionCountries <- regionCountriesDf$countryIso3
    names(regionCountries) <- regionCountriesDf$country

    return(regionCountries)
  })

  estimationTypeChoices <- reactive({
    estimationTypeChoices <- c("sliding window" = "Cori_slidingWindow", "step-wise constant" = "Cori_step")
    names(estimationTypeChoices) <- sapply(names(estimationTypeChoices), i18n()$t,  USE.NAMES = FALSE)
    return(estimationTypeChoices)
  })

  caseAverageChoices <- reactive({
    caseAverageChoices <- c("daily case numbers" = 1, "7-day average" = 7)
    names(caseAverageChoices) <- sapply(names(caseAverageChoices), i18n()$t,  USE.NAMES = FALSE)
    return(caseAverageChoices)
  })

  output$plotOptionsUI <- renderUI({
    validate(need(input$tabs, ""))
    if (input$tabs == "plots") {
      ui <- tagList(
        radioButtons("estimationTypeSelect", i18n()$t("Select estimation type to show"),
            choices = estimationTypeChoices(),
            selected = "Cori_slidingWindow", inline = FALSE),
        radioButtons("caseAverage", i18n()$t("Display case data as ..."),
          choices = caseAverageChoices(),
          selected = 1, inline = FALSE),
        checkboxGroupInput("plotOptions", label = i18n()$t("More Options"),
          choices = c(
            "Logarithmic axis for cases" = "logCases",
            "Normalize cases to per 100'000 inhabitants" = "caseNormalize",
            "Show smoothed data (Loess Fit)" = "caseLoess",
            "Show estimated infection times (deconvolution)" = "caseDeconvoluted")
        )
      )
    } else {
      ui <- tagList(
        regionCheckboxInput("regionCountrySelect", label = i18n()$t("Display regional data"),
          choices = regionCountries(), selected = "", zoomLabel = "Zoom")
      )
    }
    return(ui)
  })

# Map plot
  # data
    worldMapData <- reactive({
      allData <- allData()

      estimates <- allData$estimates %>%
        filter(
          data_type == input$dataTypeSelect,
          estimate_type == input$estimationTypeSelect) %>%
        group_by(region) %>%
        filter(date == max(date)) %>%
        dplyr::select(
            ADM0_A3_IS = countryIso3,
            region = region,
            estimate_type,
            data_typeEstimate = data_type,
            dateEstimates = date,
            median_R_mean,
            median_R_highHPD,
            median_R_lowHPD)

      worldMapData <-  allData$caseData %>%
        bind_rows() %>%
        ungroup() %>%
        filter(
          data_type == input$dataTypeSelect) %>%
        arrange(countryIso3, region, data_type, date) %>%
        group_by(region) %>%
        mutate(
          sum14days = slide_index_dbl(value, date, sum, .before = lubridate::days(14))
        ) %>%
        ungroup() %>%
        mutate(cases14d = sum14days / populationSize * 100000) %>%
        dplyr::select(
          ADM0_A3_IS = countryIso3,
          region = region,
          sourceCases = source,
          data_typeCases = data_type,
          dateCases = date,
          nCases = value,
          cases14d,
          populationSize) %>%
        group_by(ADM0_A3_IS) %>%
        filter(dateCases == max(dateCases)) %>%
        left_join(estimates, by = c("ADM0_A3_IS", "region")) %>%
        ungroup()

      return(worldMapData)
    })

  # shapeFiles
    countriesShape <- reactive({
      worldMapData <- worldMapData()

      countriesShape <- st_read("data/geoData/ne_50m_admin_0_countries.shp", quiet = TRUE)

      countriesShape <- left_join(
        countriesShape,
        filter(worldMapData, region == ADM0_A3_IS),
        by = "ADM0_A3_IS")

      return(countriesShape)
    })

    CHEregionsShape <- reactive({
      worldMapData <- worldMapData()
      CHEregionsShape <- st_read("data/geoData/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp", quiet = TRUE) %>%
        st_transform(st_crs(countriesShape())) %>%
        st_zm()

      cantonNames <- tibble(
        region = c(
          "ZH", "BE", "LU", "UR", "SZ",
          "OW", "NW", "GL", "ZG", "FR",
          "SO", "BS", "BL", "SH", "AR",
          "AI", "SG", "GR", "AG", "TG",
          "TI", "VD", "VS", "NE", "GE", "JU"),
        KANTONSNUM = c(
          1:26
        )
      )

      CHEregionsShape <- left_join(
        CHEregionsShape,
        cantonNames,
        by = "KANTONSNUM"
      )
      CHEregionsShape$ADM0_A3_IS <- "CHE"

      CHEregionsShape <- left_join(
        CHEregionsShape,
        worldMapData,
        by = c("ADM0_A3_IS", "region")
      )
      return(CHEregionsShape)
    })

    ZAFregionsShape <- reactive({
      worldMapData <- worldMapData()
      ZAFregionsShape <- st_read("data/geoData/zaf_admbnda_adm1_2016SADB_OCHA.shp", quiet = TRUE)
      ZAFregionsShape$ADM0_A3_IS <- "ZAF"
      ZAFregionsShape$region <- ZAFregionsShape$ADM1_EN %>% recode("Nothern Cape" = "Northern Cape")
      ZAFregionsShape$NAME <- ZAFregionsShape$region

      ZAFregionsShape <- left_join(
        ZAFregionsShape,
        worldMapData,
      by = c("ADM0_A3_IS", "region"))

      return(ZAFregionsShape)
    })

  # palettes
    cases14pal <- reactive({
      cases14pal <- divergentColorPal(
        palette = c("RdYlGn"),
        domain = c(0, input$casesCutoff),
        midpoint = input$casesMidpoint,
        reverse = TRUE)
      return(cases14pal)
    })

    repal <- reactive({

      repal <- divergentColorPal(
        palette = c("RdYlGn"),
        domain = c(0, input$reCutoff),
        midpoint = input$reMidpoint,
        reverse = TRUE)
      return(repal)
    })

  # outputs

    output$mapPlot <- renderLeaflet({
      worldMapData <- worldMapData()
      countriesShape <- countriesShape()
      cases14pal <- cases14pal()
      repal <- repal()

      map <- leaflet(options = leafletOptions(minZoom = 2)) %>%
        addTiles() %>%
        addMapPane("countries", zIndex = 410) %>%
        addMapPane("region", zIndex = 420)

      if ("CHE" %in% input$regionCountrySelect) {
        CHEregionsShape <- CHEregionsShape()
        cheCasesLabels <- mapLabels(shapeFileData = CHEregionsShape, mainLabel = "cases14d")
        cheReLabels <- mapLabels(shapeFileData = CHEregionsShape, mainLabel = "re")

        # remove CHE from countries

        countriesShape <- filter(countriesShape, ADM0_A3_IS != "CHE")

        map <- map %>%
          addPolygonLayer(
            shapeFile = CHEregionsShape,
            fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
            labels = cheCasesLabels,
            options = pathOptions(pane = "region")) %>%
          addPolygonLayer(
            shapeFile = CHEregionsShape,
            fillColor = ~repal(median_R_mean), group = "median Re",
            labels = cheReLabels,
            options = pathOptions(pane = "region"))
      }

      if ("ZAF" %in% input$regionCountrySelect) {
        ZAFregionsShape <- ZAFregionsShape()

        zafCasesLabels <- mapLabels(shapeFileData = ZAFregionsShape, mainLabel = "cases14d")
        zafReLabels <- mapLabels(shapeFileData = ZAFregionsShape, mainLabel = "re")

          # remove ZAF from countries
        countriesShape <- filter(countriesShape, ADM0_A3_IS != "ZAF")

        map <- map %>%
          addPolygonLayer(
            shapeFile = ZAFregionsShape,
            fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
            labels = zafCasesLabels,
            options = pathOptions(pane = "region")) %>%
          addPolygonLayer(
            shapeFile = ZAFregionsShape,
            fillColor = ~repal(median_R_mean),
            group = "median Re",
            labels = zafReLabels,
            options = pathOptions(pane = "region"))
      }

      countryCasesLabels <- mapLabels(shapeFileData = countriesShape, mainLabel = "cases14d")
      countryReLabels <- mapLabels(shapeFileData = countriesShape, mainLabel = "re")

      map <- map %>%
        addPolygonLayer(
          shapeFile = countriesShape,
          fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
          labels = countryCasesLabels) %>%
        addPolygonLayer(
          shapeFile = countriesShape,
          fillColor = ~repal(median_R_mean), group = "median Re",
          labels = countryReLabels) %>%
        addEasyButton(easyButton(
          icon = "fa-globe", title = "Reset Zoom",
          onClick = JS("function(btn, map) { map.setZoom(2); }"))) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Locate Me",
          onClick = JS("function(btn, map) { map.locate({setView: true}); }"))) %>%
        setMaxBounds(lng1 = 272.1094, lat1 = 84.73839, lng2 = -222.5391, lat2 = -71.74643) %>%
        addScaleBar(position = "bottomleft") %>%
        addLayersControl(
          baseGroups = c("Cases / 100'000 / 14 d", "median Re"),
          options = layersControlOptions(collapsed = FALSE, hideSingleBase = TRUE)
        ) %>%
        setView(
          lng = stateVals$mapViewCenter$lng, lat = stateVals$mapViewCenter$lat,
          zoom = stateVals$mapViewZoom) %>%
        hideGroup(isolate(selectedMapGroup$groups)) %>%
        showGroup(isolate(selectedMapGroup$group))

      if (isolate(selectedMapGroup$group) == "Cases / 100'000 / 14 d") {
        map <- map %>%
          addLegend(
            pal = cases14pal, opacity = 0.9, title = "Cases / 100'000 / 14 d",
            values = seq(0, input$casesCutoff, 100),
            labFormat = casesLegendLabels,
            data = countriesShape(),
            position = "bottomright", group = "Cases / 100'000 / 14 d", layerId = "casesLegend")
      } else {
        map <- map %>%
          addLegend(pal = repal, opacity = 0.9, title = "Most recent R<sub>e</sub> estimate",
            values = c(seq(0, input$reCutoff, 0.2)),
            data = countriesShape(),
            position = "bottomright", group = "median Re", layerId = "reLegend")
      }

      return(map)
    })

    output$mapPlotUI <- renderUI({
      tabBox(width = 12,
        title = tagList(shiny::icon("map"),
        HTML(i18n()$t(str_c("Map")))),
        tabPanel(
          title = "World Map",
          value = "worldMap",
          leafletOutput("mapPlot", width = "100%", height = 800) %>% withSpinner()
        )
      )
    })

    output$mapHist <- renderPlotly({
      validate(need(input$mapPlot_groups, ""))
      countriesShape <- countriesShape()

      if (input$mapPlot_groups == "Cases / 100'000 / 14 d") {
        midpoint <- input$casesMidpoint
        cutoff <- input$casesCutoff
        histPal <- cases14pal()
        binwidth <- 10
        histDataRaw <- countriesShape %>%
          as_tibble() %>%
          rename(variable = cases14d)
        title <- "Cases / 100'000 / 14 d"
      } else {
        midpoint <- input$reMidpoint
        cutoff <- input$reCutoff
        histPal <- repal()
        binwidth <- 0.1
        histDataRaw <- countriesShape %>%
          as_tibble() %>%
          rename(variable = median_R_mean)
        title <- "median R<sub>e</sub>"
      }

      histData <- histDataRaw %>%
        mutate(bins = cut(variable,
          breaks = seq(0, max(variable, na.rm = TRUE) + binwidth, binwidth))) %>%
        group_by(bins) %>%
        summarize(
          n = n(),
          countries = str_c(ADM0_A3_IS, collapse = ", "),
          .groups = "keep"
        ) %>%
        ungroup() %>%
        complete(bins, fill = list(n = 0, countries = "", color = "gray")) %>%
        mutate(
          midpoint = seq(binwidth / 2, by = binwidth, length.out = length(bins)),
          color = histPal(midpoint)) %>%
        filter(!is.na(bins))

      quantiles <- quantile(histDataRaw$variable, na.rm = TRUE)
      quantilesText <- glue::glue(
        "<b>Quantiles</b><br>",
        " min: {round(quantiles[1], 2)}<br>",
        "0.25: {round(quantiles[2], 2)}<br>",
        "0.50: {round(quantiles[3], 2)}<br>",
        "0.75: {round(quantiles[4], 2)}<br>",
        " max: {round(quantiles[5], 2)}<br>"
      )

      plot <- plot_ly(data = histData) %>%
        add_bars(x = ~midpoint, y = ~n, color = ~bins, colors = ~color,
          text = ~str_trunc(countries, 50),
          hoverinfo = "text",
          showlegend = FALSE) %>%
          add_segments(x = midpoint, xend = midpoint, y = 0, yend = max(histData$n),
            showlegend = FALSE) %>%
          add_segments(x = cutoff, xend = cutoff, y = 0, yend = max(histData$n),
            showlegend = FALSE) %>%
        layout(
          xaxis = list(range = c(0, 2 * cutoff), fixedrange = TRUE, title = title),
          yaxis = list(fixedrange = TRUE),
          annotations = list(list(
            x = 1, y = 1, xref = "paper", yref = "paper",
            width = 100,
            height = 100,
            text = quantilesText,
            valign = "top",
            showarrow = FALSE,
            xanchor = "right", yanchor = "top", align = "left",
            # xshift = helpBoxShift[1], yshift = helpBoxShift[2],
            font = list(size = 12, color = "black")
          ))) %>%
        config(displaylogo = FALSE, modeBarButtons = list(list("toImage")),
          toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1, filename = "histogram"))

      return(plot)
    })

    output$mapMethodsUI <- renderUI({
      methodsFileName <- "md/methodsOnly_"
      ui <- box(width = 8, includeMarkdown(str_c(methodsFileName, input$lang, ".md")))
      return(ui)
    })

  # map observers
    # switch legend (workaround for baseGroup limitation)
      selectedMapGroup <- reactiveValues(
        group = "Cases / 100'000 / 14 d",
        groups = c("Cases / 100'000 / 14 d", "median Re")
      )

      observeEvent(input$mapPlot_groups, {
        mapPlot <- leafletProxy("mapPlot")
        selectedMapGroup$group <- input$mapPlot_groups[1]
        if (selectedMapGroup$group == "Cases / 100'000 / 14 d") {
          mapPlot %>%
            removeControl("reLegend") %>%
            addLegend(
              pal = cases14pal(), opacity = 0.9, title = "Cases / 100'000 / 14 d",
              values = c(seq(0, input$casesCutoff, 100)),
              labFormat = casesLegendLabels,
              data = countriesShape(),
              position = "bottomright", group = "Cases / 100'000 / 14 d", layerId = "casesLegend") %>%
            hideGroup(selectedMapGroup$groups) %>%
            showGroup(selectedMapGroup$group)
        }
        else if (selectedMapGroup$group == "median Re") {
          mapPlot %>%
            removeControl("casesLegend") %>%
            addLegend(pal = repal(), opacity = 0.9, title = "Most recent R<sub>e</sub> estimate",
              values = c(seq(0, input$reCutoff, 0.2)),
              data = countriesShape(),
              position = "bottomright", group = "median Re", layerId = "reLegend") %>%
            hideGroup(selectedMapGroup$groups) %>%
            showGroup(selectedMapGroup$group)
        }
      })

    # region zoom buttons
      observeEvent(input$zoomCHE, {
        mapPlot <- leafletProxy("mapPlot")
        mapPlot %>% setView(lng = 8.360596, lat = 46.84141, zoom = 8)
      })

      observeEvent(input$zoomZAF, {
        mapPlot <- leafletProxy("mapPlot")
        mapPlot %>% setView(lng = 25.53223, lat = -28.38174, zoom = 6)
      })

      observeEvent(input$regionCountrySelect, {
        if (length(input$regionCountrySelect) == 1) {
          mapPlot <- leafletProxy("mapPlot")
          if (input$regionCountrySelect == "CHE") {
            mapPlot %>% setView(lng = 8.360596, lat = 46.84141, zoom = 8)
          } else if (input$regionCountrySelect == "ZAF") {
            mapPlot %>% setView(lng = 25.53223, lat = -28.38174, zoom = 6)
          }
        }
      })


# about page
  # data
    dataSources <- reactive({
      updateDataRaw <- bind_rows(readRDS(pathToUpdataData))
      sourceInfo <- read_csv("data/dataSources.csv", col_types = cols(.default = col_character()))

      dataSources <- updateDataRaw %>%
        ungroup() %>%
        dplyr::select(countryIso3, source, data_type, lastData) %>%
        filter(
          data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths", "Stringency Index")
        ) %>%
        left_join(dplyr::select(continents, countryIso3, country), by = "countryIso3") %>%
        left_join(sourceInfo, by = "source") %>%
        group_by(source, sourceLong, url) %>%
        dplyr::summarize(
          countries = if_else(length(unique(country)) > 5, "other Countries", str_c(unique(country), collapse = ", ")),
          data_type = str_c(sapply(as.character(unique(data_type)), i18n()$t,  USE.NAMES = FALSE), collapse = ", "),
          .groups = "drop_last") %>%
        mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
        dplyr::select("Source" = source, "Description" = sourceLong,
          "Countries" = countries, "Data types" = data_type, "URL" = url)

      names(dataSources) <- sapply(unique(names(dataSources)), i18n()$t,  USE.NAMES = FALSE)
      return(dataSources)
    })

  # outputs
    output$sourcesTable <- renderDataTable({
      dataSources()
    }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))

    output$aboutUI <- renderUI({
      fluidRow(
        box(title = NULL, width = 12,
          includeMarkdown("md/about.md")
        ),
        box(title = i18n()$t("Data Sources"), width = 12,
          dataTableOutput("sourcesTable")
        )
      )
    })

  output$test <- renderPrint({
    list(
      input$mapPlot_bounds,
      input$mapPlot_zoom,
      input$mapPlot_center
    )
  })
}
