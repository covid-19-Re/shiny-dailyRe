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

  # reactive Data

  countryData <- reactive({
    validate(need(input$countrySelect != "", "Please select a country"))
    countryData <- list(caseData = list(), estimates = list())
    estimatePlotRanges <- list()
    for (iCountry in input$countrySelect) {
      iCountryData <- loadCountryData(iCountry)
      countryData$caseData[[iCountry]] <- iCountryData$caseData
      countryData$estimates[[iCountry]] <- iCountryData$estimates
      estimatePlotRanges[[iCountry]] <- iCountryData$estimateRanges[[iCountry]]
    }

    countryData$caseData <- bind_rows(countryData$caseData)
    countryData$estimates <- bind_rows(countryData$estimates) %>%
      group_by(countryIso3, data_type) %>%
      filter(
          between(date,
            left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
            right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]])
        ) %>%
      mutate(data_type = as.character(data_type))

    return(countryData)
  })

  # allData <- reactive({
  #   allData <- list(caseData = list(), estimates = list())
  #   estimatePlotRanges <- list()
  #   allCountries <- str_match(
  #     string = list.files(path = pathToCountryData, pattern = ".*-Estimates", recursive = TRUE),
  #     pattern = ".*/(.*)-.*")[, 2]
  #   for (iCountry in allCountries) {
  #     iCountryData <- loadCountryData(iCountry)
  #     allData$caseData[[iCountry]] <- iCountryData$caseData
  #     allData$estimates[[iCountry]] <- iCountryData$estimates
  #     estimatePlotRanges[[iCountry]] <- iCountryData$estimateRanges[[iCountry]]
  #   }

  #   allData$caseData <- bind_rows(allData$caseData)
  #   allData$estimates <- bind_rows(allData$estimates) %>%
  #     group_by(countryIso3, data_type) %>%
  #     filter(
  #         between(date,
  #           left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
  #           right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]])
  #       ) %>%
  #     mutate(data_type = as.character(data_type))

  #   return(allData)
  # })

  updateData <- reactive({
    updateDataRaw <- readRDS(pathToUpdataData)

    updateData <- bind_rows(updateDataRaw[input$countrySelect])
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

  output$mapPlot <- renderLeaflet({

    estimates <- read_rds("data/countryData/recentEstimates.rds") %>%
      bind_rows() %>%
      ungroup() %>%
      filter(
        region == countryIso3,
        data_type == input$dataTypeSelect,
        estimate_type == input$estimationTypeSelect) %>%
      select(
          iso_a3 = countryIso3,
          estimate_type,
          data_typeEstimate = data_type,
          dateEstimates = date,
          median_R_mean,
          median_R_highHPD,
          median_R_lowHPD) 

    caseData <- read_rds("data/countryData/recentCases.rds") %>%
      bind_rows() %>%
      ungroup() %>%
      filter(
        region == countryIso3,
        data_type == input$dataTypeSelect,) %>%
      mutate(cases100000 = value / populationSize * 100000) %>%
      select(
        iso_a3 = countryIso3,
        sourceCases = source,
        data_typeCases = data_type,
        dateCases = date,
        nCases = value,
        cases100000,
        populationSize)

    worldmapRaw <- geojsonio::geojson_read("data/worldgeo.json", what = "sp")
    
    worldmap <- sp::merge(worldmapRaw, caseData, all.x = TRUE)
    worldmap <- sp::merge(worldmap, estimates, all.x = TRUE)

    pal <- colorNumeric("viridis", domain = worldmap$cases100000)

    labels <- as_tibble(worldmap) %>%
      str_glue_data(
        "<strong>{name}</strong><br>",
        "{round(cases100000,3)} cases / 100'000<br>",
         "R<sub>e</sub> ({dateEstimates}): {round(median_R_mean, 3)} ",
         "({round(median_R_lowHPD, 3)} - {round(median_R_highHPD, 3)})"
      )

    labels[str_detect(labels, "NA")] <- str_replace(
      str_extract_all(labels[str_detect(labels, "NA")], pattern = "(<strong>.*</strong><br>)NA"), "NA",
      "No data available")

    labels <- labels %>%
      lapply(htmltools::HTML)

    mapPlot <- leaflet(data = worldmap) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(cases100000),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
        addLegend(pal = pal, values = ~cases100000, opacity = 0.7, title = NULL,
          position = "bottomright")
    mapPlot
    return(mapPlot)
  })

  # ui
  dataTypeChoices <- reactive({
    validate(need(!is.null(input$plotTabs), ""))

    if (input$plotTabs != "data_type") {
      splitBy <- "region"
    } else {
      splitBy <- "countryIso3"
    }

    dataTypes <- countryData()$estimates %>%
      group_by(.data[[splitBy]]) %>%
      group_split() %>%
      lapply(function(x) {unique(x$data_type)})
    dataTypeChoices <- dataTypes[[which.min(lengths(dataTypes))]]
    names(dataTypeChoices) <- sapply(dataTypeChoices, i18n()$t,  USE.NAMES = FALSE)

    return(dataTypeChoices)
  })

  observeEvent(input$countrySelect, {
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
      menuItem(HTML("Plots"), tabName = "plots", icon = icon("chart-area")),
      uiOutput("linePlotOptionsUI"),
      menuItem("Map Plot", tabName = "mapPlot", icon = icon("map")),
      menuItem("Options", startExpanded = TRUE,
        uiOutput("plotOptionsUI")
      ),
      menuItem("About", tabName = "about", icon = icon("question-circle")),
      selectInput("lang", i18n()$t("Language"),
        languageSelect, selected = input$lang, multiple = FALSE,
        selectize = TRUE, width = NULL, size = NULL)
    )
  })

  output$linePlotOptionsUI <- renderUI({
    validate(need(input$tabs, ""))
    if(input$tabs == "plots") {
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
        div(style = "display:inline-block;", actionLink("africaSelect", "Africa")),
        div(style = "display:inline-block;", actionLink("europeSelect", "Europe"))
      )
    )
    return(ui)
  })

  observeEvent(input$africaSelect, {
    updateSelectizeInput(session, "countrySelect", selected = countryList$Africa)
  })
  observeEvent(input$europeSelect, {
    updateSelectizeInput(session, "countrySelect", selected = countryList$Europe)
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
        checkboxGroupInput("plotOptions", label = "more Options",
          choices = c(
            "Logarithmic axis for cases" = "logCases",
            "Normalize cases to per 100'000 inhabitants" = "caseNormalize",
            "Show smoothed data (Loess Fit)" = "caseLoess",
            "Show estimated infection times (deconvolution)" = "caseDeconvoluted")
        )
      )
    } else {
      ui <- tagList(
        radioButtons("estimationTypeSelect", i18n()$t("Select estimation type to show"),
            choices = estimationTypeChoices(),
            selected = "Cori_slidingWindow", inline = FALSE),
        radioButtons("dataTypeSelect", i18n()$t("Select data type for comparison"),
            choices = i18n()$t("Confirmed cases"),
            selected = "Confirmed cases", inline = FALSE)
      )
    }
    return(ui)
  })

  output$plotUI <- renderUI({
    fluidRow(uiOutput("plotTabsUI"))
  })
 
  output$plotTabsUI <- renderUI({
    if (length(input$countrySelect) == 1) {
      if (input$countrySelect == "CHE") {
        tabList <- list(
          c(name = "data_type", title = i18n()$t("Switzerland")),
          c(name = "region", title = i18n()$t("Canton")),
          c(name = "greaterRegion", title = i18n()$t("Greater regions")))
      } else if (input$countrySelect == "ZAF") {
        tabList <- list(
          c(name = "data_type", title = i18n()$t("South Africa")),
          c(name = "region", title = i18n()$t("Province")))
      } else {
        tabList <- list(
          c(name = "data_type", title = i18n()$t(popData$country[popData$countryIso3 == input$countrySelect])))
      }
    } else if (length(input$countrySelect) == 0) {
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
      HTML(i18n()$t(str_c("Estimation of the effective reproductive number (R<sub>e</sub>)"))))
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
    validate(need(input$countrySelect, ""))
    infoBox(width = 12,
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
    validate(need(input$countrySelect, ""))
    if (length(input$countrySelect) == 1 & input$countrySelect == "CHE") {
      methodsFileName <- "md/methodsCH_"
    } else {
      methodsFileName <- "md/methodsOnly_"
    }

    ui <- box(width = 12, includeMarkdown(str_c(methodsFileName, input$lang, ".md")))
    return(ui)
  })

  output$mapPlotUI <- renderUI({
    tabBox(width = 12,
      title = tagList(shiny::icon("map"),
      HTML(i18n()$t(str_c("SARS-CoV2 cases / 100'000 people")))),
      tabPanel(
        title = "World Map",
        value = "worldMap",
        leafletOutput("mapPlot", width = "100%", height = 800)
      )
    )
  })
}
