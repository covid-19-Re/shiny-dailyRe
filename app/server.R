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

  output$mapPlot <- renderLeaflet({
    allData <- allData()

    estimates <- allData$estimates %>%
      filter(
        region == countryIso3,
        data_type == input$dataTypeSelect,
        estimate_type == input$estimationTypeSelect) %>%
      group_by(countryIso3) %>%
      filter(date == max(date)) %>%
      select(
          ADM0_A3_IS = countryIso3,
          estimate_type,
          data_typeEstimate = data_type,
          dateEstimates = date,
          median_R_mean,
          median_R_highHPD,
          median_R_lowHPD)

    caseData <-  allData$caseData %>%
      bind_rows() %>%
      ungroup() %>%
      filter(
        region == countryIso3,
        data_type == input$dataTypeSelect) %>%
      arrange(countryIso3, region, data_type, date) %>%
      group_by(countryIso3) %>%
      mutate(
        sum14days = slide_index_dbl(value, date, sum, .before = lubridate::days(14))
      ) %>%
      ungroup() %>%
      mutate(cases14d100000 = sum14days / populationSize * 100000) %>%
      select(
        ADM0_A3_IS = countryIso3,
        sourceCases = source,
        data_typeCases = data_type,
        dateCases = date,
        nCases = value,
        cases14d100000,
        populationSize) %>%
      group_by(ADM0_A3_IS) %>%
      filter(dateCases == max(dateCases)) %>%
      left_join(estimates, by = "ADM0_A3_IS")

    countriesShape <- rgdal::readOGR(
      dsn = "data/geoData/",
      layer = "ne_50m_admin_0_countries",
      stringsAsFactors = FALSE)
    countriesShape@data <- left_join(countriesShape@data, caseData, by = "ADM0_A3_IS")

    pal <- colorBin("RdYlGn",
      bins = c(seq(0, 500, 50), Inf),
      domain = countriesShape@data$cases14d100000,
      reverse = TRUE)

    labels <- as_tibble(countriesShape@data) %>%
      mutate(
        label1 = str_c("<strong>", NAME, "</strong>"),
        label2 = if_else(is.na(cases14d100000),
          "<br>No data available",
          str_c("<br>", round(cases14d100000, 3), " cases / 100'000 (", dateCases, ")")),
        label3 = if_else(is.na(median_R_mean),
          "<br>No R<sub>e</sub> estimate available",
          str_c("<br>R<sub>e</sub> (", dateEstimates, "): ", round(median_R_mean, 3), " ",
         "(", round(median_R_lowHPD, 3), " - ", round(median_R_highHPD, 3), ")"))
        ) %>%
      transmute(
        label = str_c(label1, label2, label3)) %>%
      .$label %>%
      lapply(htmltools::HTML)

    mapPlot <- leaflet(data = countriesShape) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(cases14d100000),
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
        addLegend(pal = pal, values = seq(0, 500, 100), opacity = 0.7, title = NULL,
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
        div(style = "display:inline-block;", actionLink("africaSelect", "Africa")),
        div(style = "display:inline-block;", actionLink("asiaSelect", "Asia")),
        div(style = "display:inline-block;", actionLink("europeSelect", "Europe")),
        div(style = "display:inline-block;", actionLink("northAmericaSelect", "North America")),
        div(style = "display:inline-block;", actionLink("oceaniaSelect", "Oceania")),
        div(style = "display:inline-block;", actionLink("southAmericaSelect", "South America")),
        div(style = "display:inline-block;", actionLink("clearSelect", "Clear all")),
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
}
