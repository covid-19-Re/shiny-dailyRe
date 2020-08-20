server <- function(input, output, session) {

  stateVals <- reactiveValues(
    lang = "en-gb", tabs = "ch", sidebarExpanded = "chMenu", lastMapGroup = "median Re")

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

  # reactive Data
  countrySelectValue <- reactive({
    input$countrySelect
  }) %>% debounce(500)

  allData <- reactive({
    allData <- readRDS(pathToAllCountryData)
    return(allData)
  })

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

    updateData <- bind_rows(updateDataRaw[countrySelectValue()])
    return(updateData)
  })

  dataSources <- reactive({
    updateDataRaw <- bind_rows(readRDS(pathToUpdataData))
    sourceInfo <- read_csv("data/dataSources.csv", col_types = cols(.default = col_character()))

    dataSources <- updateDataRaw %>%
      ungroup() %>%
      dplyr::select(country, source, data_type, lastData) %>%
      filter(data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")) %>%
      left_join(sourceInfo, by = "source") %>%
      group_by(source, sourceLong, url) %>%
      dplyr::summarize(
        countries = str_c(unique(country), collapse = ", "),
        data_type = str_c(sapply(as.character(unique(data_type)), i18n()$t,  USE.NAMES = FALSE), collapse = ", "),
        .groups = "drop_last") %>%
      mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
      dplyr::select("Source" = source, "Description" = sourceLong,
        "Countries" = countries, "Data types" = data_type, "URL" = url)

    names(dataSources) <- sapply(unique(names(dataSources)), i18n()$t,  USE.NAMES = FALSE)
    return(dataSources)
  })

  output$sourcesTable <- renderDataTable({
    dataSources()
  }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))

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

  # outputs

  output$rePlot_data_type <- renderPlotly({
    countryData <- countryData()
    updateData <- updateData()
    interventions <- interventions()

    rEffPlotlyShiny(countryData, updateData, interventions, "data_type", input, i18n())
  })

  output$rePlot_region <- output$rePlot2 <- output$rePlot3 <- renderPlotly({
    countryData <- countryData()
    updateData <- updateData()
    interventions <- interventions()

    rEffPlotlyShiny(countryData, updateData, interventions, "region", input, i18n())
  })

  output$rePlot_greaterRegion <- renderPlotly({
    countryData <- countryData()
    updateData <- updateData()
    interventions <- interventions()

    rEffPlotlyShiny(countryData, updateData, interventions, "greaterRegion", input, i18n())
  })

  worldMapData <- reactive({
    allData <- allData()

    estimates <- allData$estimates %>%
      filter(
        data_type == input$dataTypeSelect,
        estimate_type == input$estimationTypeSelect) %>%
      group_by(region) %>%
      filter(date == max(date)) %>%
      select(
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
      select(
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

  output$mapPlot <- renderLeaflet({
    worldMapData <- worldMapData()

    countriesShape@data <- left_join(
      countriesShape@data,
      filter(worldMapData, region == ADM0_A3_IS),
      by = "ADM0_A3_IS")
    countryCasesLabels <- mapLabels(shapeFileData = countriesShape@data, mainLabel = "cases14d")
    countryReLabels <- mapLabels(shapeFileData = countriesShape@data, mainLabel = "re")

    cases14pal <- colorBin("RdYlGn",
      bins = c(seq(0, 500, 50), Inf),
      domain = countriesShape@data$cases14d,
      reverse = TRUE)
    repal <- colorNumeric("RdYlGn",
      domain = countriesShape@data$median_R_mean,
      reverse = TRUE)

    map <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      addMapPane("countries", zIndex = 410) %>%
      addMapPane("region", zIndex = 420) 

    if ("CHE" %in% input$regionCountrySelect) {
      CHEregionsShape@data <- left_join(
        CHEregionsShape@data,
        worldMapData,
        by = c("ADM0_A3_IS", "region")
      )
      cheCasesLabels <- mapLabels(shapeFileData = CHEregionsShape@data, mainLabel = "cases14d")
      cheReLabels <- mapLabels(shapeFileData = CHEregionsShape@data, mainLabel = "re")

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
      ZAFregionsShape@data <- left_join(
        ZAFregionsShape@data,
        worldMapData,
      by = c("ADM0_A3_IS", "region"))

      zafCasesLabels <- mapLabels(shapeFileData = ZAFregionsShape@data, mainLabel = "cases14d")
      zafReLabels <- mapLabels(shapeFileData = ZAFregionsShape@data, mainLabel = "re")

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
    
    map <- map %>%
      addPolygonLayer(
        shapeFile = countriesShape,
        fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
        labels = countryCasesLabels) %>%
      addLegend(pal = cases14pal, values = c(seq(0, 500, 50), Inf), opacity = 0.7, title = "Cases / 100'000 / 14 d",
        position = "bottomright", group = "Cases / 100'000 / 14 d") %>%
      addPolygonLayer(
        shapeFile = countriesShape,
        fillColor = ~repal(median_R_mean), group = "median Re",
        labels = countryReLabels) %>%
      addLegend(pal = repal, opacity = 0.7, title = "Most recent R<sub>e</sub> estimate",
        values = ~median_R_mean, data = countriesShape, ,
        position = "bottomright", group = "median Re") %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset Zoom",
        onClick = JS("function(btn, map) { map.setZoom(2); }"))) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map) { map.locate({setView: true}); }"))) %>%
      fitBounds(lng1 = 272.1094, lat1 = 84.73839, lng2 = -222.5391, lat2 = -71.74643) %>%
      setMaxBounds(lng1 = 272.1094, lat1 = 84.73839, lng2 = -222.5391, lat2 = -71.74643) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        overlayGroups = c("median Re", "Cases / 100'000 / 14 d"),
        options = layersControlOptions(collapsed = FALSE, hideSingleBase = TRUE)
      )

    return(map)
  })

  observeEvent(input$mapPlot_groups, {
    mymap <- leafletProxy("mapPlot")
    isolate({
      if (length(input$mapPlot_groups) > 1) {
        mymap %>%
          hideGroup(stateVals$lastMapGroup)
        stateVals$lastMapGroup <- if_else(
          stateVals$lastMapGroup == "median Re",
            "Cases / 100'000 / 14 d",
            "median Re"
          )
      } else {
        if (input$mapPlot_groups == "median Re") {
          stateVals$lastMapGroup <- "median Re"
          mymap %>%
            hideGroup("Cases / 100'000 / 14 d")
        }
        else if (input$mapPlot_groups == "Cases / 100'000 / 14 d") {
          stateVals$lastMapGroup <- "Cases / 100'000 / 14 d"
          mymap %>%
            hideGroup("median Re")
        }
      }
    })
  })

  # ui
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
        lapply(function(x) {unique(x$data_type)})
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

  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem(HTML(i18n()$t("Timeseries")), tabName = "plots", icon = icon("chart-area")),
      uiOutput("linePlotOptionsUI"),
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

  output$linePlotOptionsUI <- renderUI({
    validate(need(input$tabs, ""))
    if (input$tabs == "plots") {
      ui <- tagList(
        selectizeInput("countrySelect", i18n()$t("Countries"),
          countryList,
          selected = "CHE", multiple = TRUE, width = "100%", size = NULL,
          options = list(plugins = list("remove_button"))
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

  output$quickselectUI <- renderUI({
    ui <- tagList(
      HTML("<div class='form-group shiny-input-container' style='width: 100%;'>
      <label class='control-label' for='quickselectButtons'>Choose all countries in continent:</label>"),
      div(
        div(class = "quickSelect", actionLink("africaSelect", i18n()$t("Africa"))),
        div(class = "quickSelect", actionLink("asiaSelect", i18n()$t("Asia"))),
        div(class = "quickSelect", actionLink("europeSelect", i18n()$t("Europe"))),
        div(class = "quickSelect", actionLink("northAmericaSelect", i18n()$t("North America"))),
        div(class = "quickSelect", actionLink("oceaniaSelect", i18n()$t("Oceania"))),
        div(class = "quickSelect", actionLink("southAmericaSelect", i18n()$t("South America"))),
        div(class = "quickSelect", actionLink("clearSelect", i18n()$t("Clear all"))),
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

  regionCountries <- reactive({
    allData <- allData()

    regionCountriesDf <- allData$caseData %>%
      group_by(countryIso3) %>%
      filter(length(unique(region)) > 1) %>%
      select(countryIso3, country) %>%
      distinct()
    
    regionCountries <- regionCountriesDf$countryIso3
    names(regionCountries) <- regionCountriesDf$country

    return(regionCountries)
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
        selectizeInput("regionCountrySelect", i18n()$t("Display regional data for countries"),
          regionCountries(),
          selected = NULL, multiple = TRUE, width = "100%", size = NULL,
          options = list(plugins = list("remove_button"))
        )
      )
    }
    return(ui)
  })

  output$plotUI <- renderUI({
    fluidRow(uiOutput("plotTabsUI"))
  })

  output$plotTabsUI <- renderUI({
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
          plotlyOutput(str_c("rePlot_", i[[1]]), width = "100%", height = "800px"),
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

    ui <- box(width = 8, includeMarkdown(str_c(methodsFileName, input$lang, ".md")))
    return(ui)
  })

  output$mapPlotUI <- renderUI({
    tabBox(width = 12,
      title = tagList(shiny::icon("map"),
      HTML(i18n()$t(str_c("Map")))),
      tabPanel(
        title = "World Map",
        value = "worldMap",
        leafletOutput("mapPlot", width = "100%", height = 800)
      )
    )
  })

  output$test <- renderPrint({
     list(
       input$mapPlot_groups)
  })
}
