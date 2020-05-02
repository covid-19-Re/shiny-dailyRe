server <- function(input, output, session) {

  # load data
  load(pathToRawData)
  load(pathToEstimatesRePlot)
  load(pathTolastDataDate)
  load(pathToCantonList)

  lastCheck <- readLines(pathToLastCheck)
  
  interventions <- read_csv(pathToInterventionData,
    col_types = cols(
      name = col_character(),
      y = col_double(),
      text = col_character(),
      tooltip = col_character(),
      type = col_character(),
      date = col_date(format = ""),
      plotTextPosition = col_character()))

  updateSelectInput(session, "canton", choices = cantonList, selected = cantonList)

  observeEvent(input$selectAllCantons, {
    updateSelectInput(session, "canton", selected = cantonList)
  })

  observeEvent(input$methodsLink, {
    updateTabItems(session, "tabs", selected = "methods")
  })

  output$lastUpdateBox <- renderInfoBox({
    infoBox(
      "Last Data Updates",
      HTML(dataUpdatesTable(lastDataDate, lastCheck)), icon = icon("exclamation-circle"),
      color = "purple"
    )
  })

  cumulativePlotData <- reactive({
    cumulativePlotData <- filter(rawData, !(region != "CH" & data_type == "deaths")) %>%
      mutate(data_type = factor(
        data_type,
        levels = c("confirmed", "hospitalized", "deaths"),
        labels = c("Confirmed cases", "Hospitalized patients", "Deaths"))) %>%
      pivot_wider(names_from = "variable", values_from = "value")
    return(cumulativePlotData)
  })

  cumulativePlotDataFiltered <- reactive({
    cumulativePlotDataFiltered <- cumulativePlotData() %>%
      filter(region %in% input$canton)
    return(cumulativePlotDataFiltered)
  })

  output$cumulativePlot <- renderPlot({
    startDateA <- as.Date("2020-02-25")
    endDateA <- Sys.Date() - 1

    pCases <- ggplot(
        data = cumulativePlotDataFiltered(),
        mapping = aes(x = date)) +
      facet_grid(region ~.) +
      geom_line(
        mapping = aes(y = cumul, group = data_type, color = data_type),
        size = 1) + 
      geom_bar(
        mapping = aes(y = incidence, fill = data_type),
        stat = "identity",
        position = position_dodge(preserve = "single"), width = 1, alpha = 1) +
      scale_x_date(
        date_breaks = "6 days",
        date_labels = "%b-%d",
        limits = c(startDateA, endDateA)) +
      scale_y_log10() +
      ylab("Cumulative (line) and daily (bars) numbers") +
      xlab("") +
      colorScale +
      labs(title = "Daily and cumulative Incidence", caption = dataUpdatesString(lastDataDate)) +
      plotTheme

      return(pCases)
  }, height = function(){350 + 150 * (length(input$canton) - 1)})

  estimatesRePlotFiltered <- reactive({
    estimatesRePlotFiltered <- filter(estimatesRePlot,
      # confirmed: delay 10 days
      !(data_type == "Confirmed cases" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 10)),
      # hospitalized
      !(data_type == "Hospitalized patients" & date > (lastDataDate[lastDataDate$source == "FOPH",]$date - 10)),
      # deaths: delay 16 days
      !(data_type == "Deaths" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 15))
    )
  })

  rEffPlotWindowData <- reactive({
    rEffPlotWindowData <- filter(estimatesRePlotFiltered(),
      estimate_type == "Cori_slidingWindow")
    return(rEffPlotWindowData)
  })

  rEffPlotWindowDataFiltered <- reactive({
    rEffPlotWindowData <- filter(rEffPlotWindowData(),
      region %in% input$canton)
    return(rEffPlotWindowData)
  })
  
  output$rEffPlotWindow <- renderPlot({
    startDateB <- as.Date("2020-03-07")
    endDateB <- max(rEffPlotWindowDataFiltered()$date)

    pRe <- ggplot(
        data = rEffPlotWindowDataFiltered(),
        mapping = aes(x = date)) +
      facet_grid(region ~.) +
      geom_ribbon(
        mapping = aes(ymin = median_R_lowHPD, ymax = median_R_highHPD, fill = data_type),
          alpha = 0.7, colour = NA) +
      geom_ribbon(
        mapping = aes(ymin = lowQuantile_R_lowHPD, ymax = highQuantile_R_highHPD, fill = data_type),
        alpha = 0.15, colour = NA) +
      geom_line(
        mapping = aes(y = median_R_mean, group = data_type, color = data_type),
        size = 1.1) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_x_date(
        date_breaks = "4 days",
        date_labels = "%b%d",
        limits = c(startDateB, endDateB)) +
      coord_cartesian(ylim = c(0, 3)) +
      geom_vline(
        xintercept = c(as.Date("2020-03-14"), as.Date("2020-03-17"), as.Date("2020-03-20")),
        linetype = "dotted") +
      geom_vline(
        xintercept = c(as.Date("2020-04-13")),
        linetype = "dashed") +
      annotate("rect",
        xmin = as.Date("2020-03-14"),
        xmax = as.Date("2020-03-17"),
        ymin = -1, ymax = Inf, alpha = 0.45, fill = "grey") +
      colorScale +
      xlab("") +
      ylab("Reproductive number") +
      labs(title = "Sliding Window Estimation", caption = dataUpdatesString(lastDataDate)) +
      plotTheme

    return(pRe)
  }, height = function(){350 + 150 * (length(input$canton) - 1) }
  )

  rEffPlotStepData <- reactive({
    rEffPlotStepData <- filter(estimatesRePlotFiltered(),
      estimate_type == "Cori_step")
    return(rEffPlotStepData)
  })

  rEffPlotStepDataFiltered <- reactive({
    rEffPlotStepDataFiltered <- filter(rEffPlotStepData(),
      region %in% input$canton)
    return(rEffPlotStepDataFiltered)
  })
  
  output$rEffPlotStep <- renderPlot({
    startDateB <- as.Date("2020-03-07")
    endDateB <- max(rEffPlotStepDataFiltered()$date)

    pRe <- ggplot(
        data = rEffPlotStepDataFiltered(),
        mapping = aes(x = date)) +
      facet_grid(region ~.) +
      geom_ribbon(
        mapping = aes(ymin = median_R_lowHPD, ymax = median_R_highHPD, fill = data_type),
          alpha = 0.7, colour = NA) +
      geom_ribbon(
        mapping = aes(ymin = lowQuantile_R_lowHPD, ymax = highQuantile_R_highHPD, fill = data_type),
        alpha = 0.15, colour = NA) +
      geom_line(
        mapping = aes(y = median_R_mean, group = data_type, color = data_type),
        size = 1.1) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_x_date(
        date_breaks = "4 days",
        date_labels = "%b%d",
        limits = c(startDateB, endDateB)) +
      coord_cartesian(ylim = c(0, 3)) +
      geom_vline(
        xintercept = c(as.Date("2020-03-14"), as.Date("2020-03-17"), as.Date("2020-03-20")),
        linetype = "dotted") +
      geom_vline(
        xintercept = c(as.Date("2020-04-13")),
        linetype = "dashed") +
      annotate("rect",
        xmin = as.Date("2020-03-14"),
        xmax = as.Date("2020-03-17"),
        ymin = -1, ymax = Inf, alpha = 0.45, fill = "grey") +
      colorScale +
      xlab("") +
      ylab("Reproductive number") +
      labs(title = "Step-wise estimation", caption = dataUpdatesString(lastDataDate)) +
      plotTheme

    return(pRe)
  }, height = function(){350 + 150 * (length(input$canton) - 1)}
  )

  output$CHinteractivePlot <- renderPlotly({
    plot <- rEffPlotly(
      cumulativePlotData(),
      rEffPlotWindowData(),
      interventions,
      plotColoursNamed,
      lastDataDate,
      widgetID = NULL)
    return(plot)
  })
}
