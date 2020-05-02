server <- function(input, output, session) {

  # load data
  load(pathToRawData)
  load(pathToEstimatesRePlot)
  load(pathTolastDataDate)
  load(pathToCantonList)
  
  interventions <- read_csv(pathToInterventionData,
    col_types = cols(
      name = col_character(),
      y = col_double(),
      text = col_character(),
      tooltip = col_character(),
      type = col_character(),
      date = col_date(format = "")))

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
      HTML(dataUpdatesTable(lastDataDate)), icon = icon("exclamation-circle"),
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

  rEffPlotWindowData <- reactive({
    rEffPlotWindowData <- filter(estimatesRePlot,
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
    endDateB <- (Sys.Date() - 11)

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
    rEffPlotStepData <- filter(estimatesRePlot,
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
    endDateB <- (Sys.Date() - 11)

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
    # prepare Data
    caseData <- cumulativePlotData() %>% filter(region == "CH")

    startDate <- min(caseData$date) - 1
    endDate <- Sys.Date()
    lastDate <- max(caseData$date)
    maxEstimateDate <- max(caseData$date) - 10
    minEstimateDate <- as.Date("2020-03-07")

    rEffWindowData <- rEffPlotWindowData() %>%
      filter(
        region == "CH",
        date <= maxEstimateDate,
        date >= minEstimateDate,
        replicate == 1) %>%
      select(-replicate)

    annotateShapes <- tribble(
              ~name,            ~x, ~y,                                          ~text,                               ~tooltip,    ~lineColor, ~lineWidth,
      "asymptomatic", lastDate - 10,  0,    "~ 5 days from infection <br> to symptoms",    "R<sub>eff</sub> can only be estimated with a delay <br> because case confirmation and reporting is on <br> average 10 days after infection", "transparent",          0,
      "asymptomatic", lastDate - 10,  5,    "~ 5 days from infection <br> to symptoms",                                     NA, "transparent",          0,
      "asymptomatic",  lastDate - 5,  5,    "~ 5 days from infection <br> to symptoms",                                     NA, "transparent",          0,
      "asymptomatic",  lastDate - 5,  0,    "~ 5 days from infection <br> to symptoms",                                     NA, "transparent",          0,
          "symptoms",  lastDate - 5,  0, "~ 5 days from symptoms <br> to confirmation",    "R<sub>eff</sub> can only be estimated with a delay <br> because case confirmation and reporting is on <br> average 10 days after infection", "transparent",          0,
          "symptoms",  lastDate - 5,  5, "~ 5 days from symptoms <br> to confirmation",                                     NA, "transparent",          0,
          "symptoms",      lastDate,  5, "~ 5 days from symptoms <br> to confirmation",                                     NA, "transparent",          0,
          "symptoms",      lastDate,  0, "~ 5 days from symptoms <br> to confirmation",                                     NA, "transparent",          0,
    )
    
    annotateShapesText <- annotateShapes %>%
      group_by(name) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      add_row(
        name = "lastData", x = lastDate, y = 1, text = "Most Recent Data", tooltip = str_c("Most Recent Data <br>",lastDate), lineColor = "transparent", lineWidth = 0
      )

    pCases <- plot_ly(data = caseData) %>%
      add_bars(x = ~date, y = ~incidence, color = ~data_type,
        colors = plotColoursNamed,
        text = ~str_c("<i>", format(date, "%d.%m.%y"), "</i> <br>", incidence," ", data_type, "<extra></extra>"),
        hovertemplate = "%{text}",
        legendgroup = ~data_type) %>%
      layout(
        xaxis = list(title = "",
          type = "date",
          range = c(startDate, endDate),
          tick0 = startDate,
          dtick = 3 * 86400000,
          tickformat = "%b-%d",
          tickangle = 45,
          showgrid = TRUE),
        yaxis = list(
          # type = "log",
          fixedrange = TRUE,
          title = "Daily Incidence"),
        legend = list(title = list(text = "<b> Data Type </b>")))

    pReSlidingWindow <- plot_ly(data = rEffWindowData) %>%
      add_trace(
        x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColoursNamed,
        type = "scatter", mode = "lines",
        legendgroup = ~data_type, showlegend = F,
        text = ~str_c("<i>", format(date, "%d.%m.%y"),
        "</i> <br> R<sub>eff</sub>: ", signif(median_R_mean, 3),
        " (", signif(median_R_lowHPD, 3),"-", signif(median_R_highHPD, 3),")",
        " <br>(", data_type, ")<extra></extra>"),
        hovertemplate = "%{text}") %>%
      add_ribbons(
        x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
        color = ~data_type, colors = plotColoursNamed,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~data_type, showlegend = F,
        hoverinfo = "none") %>%
      add_polygons(
        x = annotateShapes$x, y = annotateShapes$y, color = annotateShapes$name,
        fill = "toself", showlegend = FALSE, opacity = 0.5,
        line = list(color = annotateShapes$lineColor, width = annotateShapes$lineWidth),
        text = annotateShapes$tooltip,
        hoveron = "fills",
        hoverinfo = "text") %>%
      add_annotations(
        text = annotateShapesText$text,
        x = annotateShapesText$x, y = 1,
        textangle = -90,
        xanchor = "left",
        yanchor = "middle",
        showarrow = FALSE) %>%
      add_annotations(
        text = c("exponential <br> increase","decrease"),
        font = list(color = "red"),
        x = 0,
        xrev = "paper",
        y = c(1.5, 0.5),
        textangle = -90,
        xanchor = "left",
        yanchor = "middle",
        showarrow = FALSE) %>%
      layout(
        xaxis = list(title = "",
          type = "date",
          range = c(startDate, endDate),
          tick0 = startDate,
          dtick = 3 * 86400000,
          tickformat = "%b-%d",
          tickangle = 45,
          showgrid = TRUE),
        yaxis = list(
          range = c(0, 2),
          fixedrange = TRUE,
          title = "Reproductive Number R<sub>eff</sub>",
          zeroline = TRUE),
        legend = list(title = list(text = "<b>Data Type</b>")),
        shapes = list(
          list(
            type = "line", 
            x0 = startDate, x1 = maxEstimateDate,
            y0 = 1, y1 = 1,
            line = list(color = "red", width = 0.5)
          ),
          list(
            type = "line", 
            x0 = lastDate, x1 = lastDate,
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "black", width = 4)
          )
        )
      )

    pIntervention <- plot_ly(data = interventions) %>%
      add_trace(
        x = ~date, y = ~y, color = ~name,
        type = "scatter", mode = "markers+lines",
        colors = rep("black", length(interventions$date)),
        showlegend = FALSE,
        text = ~tooltip,
        hoveron = "points",
        hoverinfo = "text") %>%
      add_text(x = ~date, y = ~y, color = ~name, text = ~text,
        textposition = "top right", showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "",
          type = "date",
          range = c(startDate, endDate),
          tick0 = startDate,
          dtick = 3 * 86400000,
          tickformat = "%b-%d",
          tickangle = 45,
          showgrid = TRUE),
        yaxis = list(visible = FALSE, fixedrange = TRUE)) 
    
    plotlist <- list(pCases, pReSlidingWindow, pIntervention)
    plot <- subplot(plotlist, nrows = 3, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
      layout(annotations = list(
          x = 1, y = -0.1,
          text = dataUpdatesString(lastDataDate),
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black"))) %>%
      config(doubleClick = "reset", displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          filename = str_c(format(Sys.time(), "%Y%m%d"), "-rEffEstimationPlot"),
          format = "png",
          width = 1000,
          height = 1000))
    return(plot)
  })
}
