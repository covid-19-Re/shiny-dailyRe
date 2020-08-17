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
      menuItem(HTML("Plots"), tabName = "plots"),
      selectizeInput("countrySelect", i18n()$t("Countries"),
        countryList,
        selected = "CHE", multiple = TRUE, width = "100%", size = NULL,
        options = list(plugins = list("remove_button"))
      ),
      conditionalPanel(
      condition = "input.plotTabs != 'data_type' | input.countrySelect.length > 1",
        radioButtons("dataTypeSelect", i18n()$t("Select data type for comparison"),
          choices = i18n()$t("Confirmed cases"),
          selected = "Confirmed cases", inline = FALSE)
      ),
      menuItem("Options",
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
      ),
      menuItem("About", tabName = "about", icon = icon("question-circle")),
      selectInput("lang", i18n()$t("Language"),
        languageSelect, selected = input$lang, multiple = FALSE,
        selectize = TRUE, width = NULL, size = NULL)
    )
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
    if (length(input$countrySelect) == 1 & input$countrySelect == "CHE") {
      methodsFileName <- "md/methodsCH_"
    } else {
      methodsFileName <- "md/methodsOnly_"
    }
    
    ui <- box(width = 12, includeMarkdown(str_c(methodsFileName, input$lang, ".md")))
    return(ui)
  })
}
