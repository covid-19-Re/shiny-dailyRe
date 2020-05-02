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

  estimatesRePlotFiltered <- reactive({
    estimatesRePlotFiltered <- filter(estimatesRePlot,
      # confirmed: delay 10 days
      !(data_type == "Confirmed cases" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 10)),
      # hospitalized
      !(data_type == "Hospitalized patients" & date > (lastDataDate[lastDataDate$source == "BAG",]$date - 10)),
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
    # prepare Data
    caseData <- cumulativePlotData() %>% filter(region == "CH")

    startDate <- min(caseData$date) - 1
    endDate <- Sys.Date()
    lastDate <- max(caseData$date)
    minEstimateDate <- as.Date("2020-03-07")

    rEffWindowData <- rEffPlotWindowData() %>%
      filter(
        region == "CH",
        date >= minEstimateDate,
        replicate == 1) %>%
      select(-replicate)

    estimatesEndPoint <- rEffWindowData %>%
      group_by(data_type) %>%
      filter(date == max(date))

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
          showgrid = TRUE,
          fixedrange = TRUE),
        yaxis = list(
          fixedrange = TRUE,
          title = "Daily Incidence"),
        legend = list(title = list(text = "<b> Data Type </b>")))

    pReSlidingWindow <- plot_ly(data = rEffWindowData) %>%
      add_trace(
        x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColoursNamed,
        type = "scatter", mode = "lines",
        legendgroup = ~data_type, showlegend = FALSE,
        text = ~str_c("<i>", format(date, "%d.%m.%y"),
        "</i> <br> R<sub>eff</sub>: ", signif(median_R_mean, 3),
        " (", signif(median_R_lowHPD, 3),"-", signif(median_R_highHPD, 3),")",
        " <br>(", data_type, ")<extra></extra>"),
        hovertemplate = "%{text}") %>%
      add_markers(
        data = estimatesEndPoint,
        x = ~date + 1, y = ~median_R_mean,
        legendgroup = ~data_type,
        marker = list(color = "black", symbol = "asterisk-open"),
        hoverinfo = "none",
        # text =  ~str_c("<i>", format(date, "%d.%m.%y"),
        #   " </i>R<sub>eff</sub>: ", signif(median_R_mean, 3),
        #   "(", signif(median_R_lowHPD, 3),"-", signif(median_R_highHPD, 3),")<br>",
        #   "<b>*</b>&nbsp;This is the most recent <br>",
        #   "R<sub>e</sub> estimate due to delays<br>",
        #   "between infection and</br>",
        #   "the last observation"),
        # hovertemplate = "%{text}",
        inherit = FALSE, showlegend = FALSE) %>%
      add_ribbons(
        x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
        color = ~data_type, colors = plotColoursNamed,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~data_type, showlegend = FALSE,
        hoverinfo = "none") %>%
      add_annotations(
        text = c("exponential <br> increase", "decrease"),
        font = list(color = "red"),
        x = startDate,
        y = c(1.5, 0.5),
        textangle = -90,
        xanchor = "left",
        yanchor = "middle",
        showarrow = FALSE,
        inherit = FALSE) %>%
      layout(
        xaxis = list(title = "",
          type = "date",
          range = c(startDate, endDate),
          tick0 = startDate,
          dtick = 3 * 86400000,
          tickformat = "%b-%d",
          tickangle = 45,
          showgrid = TRUE,
          fixedrange = TRUE),
        yaxis = list(
          range = c(0, 2),
          fixedrange = TRUE,
          title = "Reproductive Number R<sub>eff</sub>",
          zeroline = TRUE),
        legend = list(title = list(text = "<b>Data Type</b>")),
        shapes = list(
          list(
            type = "line", 
            x0 = startDate, x1 = endDate,
            y0 = 1, y1 = 1,
            line = list(color = "red", width = 0.5)
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
        textposition = ~plotTextPosition, showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "",
          type = "date",
          range = c(startDate, endDate),
          tick0 = startDate,
          dtick = 3 * 86400000,
          tickformat = "%b-%d",
          tickangle = 45,
          showgrid = TRUE,
          fixedrange = TRUE),
        yaxis = list(visible = FALSE, fixedrange = TRUE)) 
    
    plotlist <- list(pCases, pReSlidingWindow, pIntervention)
    plot <- subplot(plotlist, nrows = 3, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
      layout(annotations = list(
        list(
          x = 1, y = -0.1, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate),
          showarrow = FALSE,
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")),
        list(
          x = 1, y = 0.35, xref = "paper", yref = "paper",
          text = str_c("<b>*</b>&nbsp;This is the most recent <br>",
            "R<sub>e</sub> estimate due to delays<br>",
            "between infection and</br>",
            "the last observation"),
          showarrow = FALSE,
          xanchor = "left", yanchor = "bottom", align = "left",
          xshift = 10, yshift = 0,
          font = list(size = 11, color = "black")))) %>%
      config(doubleClick = "reset", displaylogo = FALSE, displayModeBar = FALSE,
        modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d", "autoScale2d"),
        toImageButtonOptions = list(
          filename = str_c(format(Sys.time(), "%Y%m%d"), "-rEffEstimationPlot"),
          format = "png",
          width = 1000,
          height = 1000))
    return(plot)
  })
}
