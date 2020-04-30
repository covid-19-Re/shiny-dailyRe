server <- function(input, output, session) {

  # load data
  load(pathToRawData)
  load(pathToEstimatesRePlot)
  load(pathTolastDataDate)
  
  load(pathToCantonList)
  updateSelectInput(session, "canton", choices = cantonList, selected = "CH")

  observeEvent(input$selectAllCantons, {
    updateSelectInput(session, "canton", selected = cantonList)
  })

  observeEvent(input$methodsLink, {
    updateTabItems(session, "tabs", selected = "methods")
  })

  output$lastUpdateBox <- renderInfoBox({
    infoBox(
      "Last Data Updates",
      HTML(dataUpdatesTable(lastDataDate)), icon = icon("exclamation-circle"),
      color = "purple"
    )
  })

  cumulativePlotData <- reactive({
    out <- filter(rawData, !(region!="CH" & data_type=="deaths")) %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      filter(region %in% input$canton)
    return(out)
  })

  output$cumulativePlot <- renderPlot({
    startDateA <- as.Date("2020-02-25")
    endDateA <- Sys.Date()-1

    pCases <- ggplot(
        data = cumulativePlotData(),
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
      scale_colour_manual(
        values = c("black", ggColor(3)[c(1,3)]),
        labels = c("Confirmed cases", "Hospitalizations", "Deaths"),
        breaks = c("confirmed", "hospitalized", "deaths"),
        aesthetics = c("colour", "fill")) +
      labs(title = "Daily and cumulative Incidence", caption = dataUpdatesString(lastDataDate)) +
      plotTheme

      return(pCases)
  }, height = function(){350 + 150 * (length(input$canton) - 1)})

  rEffPlotWindowData <- reactive({
    out <- filter(estimatesRePlot,
      estimate_type == "Cori_slidingWindow",
      region %in% input$canton)
    return(out)
  })
  
  output$rEffPlotWindow <- renderPlot({
    startDateB <- as.Date("2020-03-07")
    endDateB <- (Sys.Date() - 11)

    pRe <- ggplot(
        data = rEffPlotWindowData(),
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
      scale_colour_manual(
        values = c(ggColor(3)[c(1,3)], "black"),
        labels = c("Confirmed cases",  "Deaths", "Hospitalized"),
        breaks = c("infection_confirmed", "infection_deaths", "infection_hospitalized"),
        name  = "Data source",
        aesthetics = c("fill", "color")) +
      xlab("") +
      ylab("Reproductive number") +
      labs(title = "Sliding Window Estimation", caption = dataUpdatesString(lastDataDate)) +
      plotTheme

    return(pRe)
  }, height = function(){350 + 150 * (length(input$canton) - 1)}
  )
  rEffPlotStepData <- reactive({
    out <- filter(estimatesRePlot,
      estimate_type == "Cori_step",
      region %in% input$canton)
    return(out)
  })
  
  output$rEffPlotStep <- renderPlot({
    startDateB <- as.Date("2020-03-07")
    endDateB <- (Sys.Date() - 11)

    pRe <- ggplot(
        data = rEffPlotStepData(),
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
      scale_colour_manual(
        values = c(ggColor(3)[c(1,3)], "black"),
        labels = c("Confirmed cases",  "Deaths", "Hospitalized"),
        breaks = c("infection_confirmed", "infection_deaths", "infection_hospitalized"),
        name  = "Data source",
        aesthetics = c("fill", "color")) +
      xlab("") +
      ylab("Reproductive number") +
      labs(title = "Step-wise estimation", caption = dataUpdatesString(lastDataDate)) +
      plotTheme

    return(pRe)
  }, height = function(){350 + 150 * (length(input$canton) - 1)}
  )
}
