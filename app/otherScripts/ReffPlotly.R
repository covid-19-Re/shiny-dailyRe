
rEffPlotly <- function(
  cumulativePlotData,
  rEffPlotWindowData,
  interventions,
  plotColoursNamed,
  lastDataDate,
  widgetID = "rEffplots"){
  # prepare Data

  axisTitleFontSize <- 14

  caseData <- cumulativePlotData %>% filter(region == "CH")

  startDate <- min(caseData$date) - 1
  endDate <- Sys.Date()
  lastDate <- max(caseData$date)
  minEstimateDate <- as.Date("2020-03-07")

  rEffWindowData <- rEffPlotWindowData %>%
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
      text = ~str_c("<i>", format(date, "%d.%m.%y"), "</i> <br>", incidence, " ", data_type, "<extra></extra>"),
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
        title = list(text = "New observations", font = list(size = axisTitleFontSize))),
      legend = list(title = list(text = "<b> Data Type </b>")))

  pReSlidingWindow <- plot_ly(data = rEffWindowData) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColoursNamed,
      type = "scatter", mode = "lines",
      legendgroup = ~data_type, showlegend = FALSE,
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3),"-", signif(median_R_highHPD, 3),")",
      " <br>(", data_type, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~data_type, colors = plotColoursNamed,
      line = list(color = "transparent"), opacity = 0.5,
      legendgroup = ~data_type, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(data_type) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date)+10*60*60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~data_type, colors = plotColoursNamed,
      legendgroup = ~data_type,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3),")",
      " <br>(", data_type, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    # add_text(
    #   x = ~as.POSIXct(date)+20*60*60, y = ~median_R_mean + c(0, 0.3, -0.3),
    #   color = ~data_type, colors = plotColoursNamed,
    #   legendgroup = ~data_type, showlegend = FALSE,
    #   text = ~str_c("R<sub>e</sub> = ", signif(median_R_mean, 3),
    #   " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3),")"),
    #   textposition = "middle right",
    #   cliponaxis = FALSE,
    #   hoverinfo = "none"
    # ) %>%
    add_annotations(
      text = c("↑ exponential increase<br>in number of new cases", "↓ decrease in number of new cases"),
      font = list(color = "red"),
      x = startDate,
      y = c(1.2, 0.9),
      textangle = 0,
      align = "left",
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
        title = list(
          text = "Reproductive number R<sub>e</sub>",
          font = list(size = axisTitleFontSize)),
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
      colors = rep("#505050", length(interventions$date)),
      showlegend = FALSE,
      text = ~str_c("<i>", date, "</i><br>", tooltip),
      hoveron = "points",
      hoverinfo = "text") %>%
    add_text(x = ~date, y = ~y, color = ~name, text = ~text,
      textposition = ~plotTextPosition, showlegend = FALSE, textfont = list(size = 10)) %>%
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
        text = str_c("<b>*</b>&nbsp;This is the most recent<br>",
          "possible R<sub>e</sub> estimate due to <br>",
          "delays between infection and</br>",
          "the last data observation"),
        showarrow = FALSE,
        xanchor = "left", yanchor = "bottom", align = "left",
        xshift = 10, yshift = 0,
        font = list(size = 11, color = "black")),
      list(
        x = 1, y = 0.8, xref = "paper", yref = "paper",
        width = 174,
        height = 90,
        bgcolor = "#eeeeee",
        text = str_c(
          "&nbsp;<b>Interactive Plot</b><br>",
          "&nbsp;&nbsp;• click legend to toggle<br>",
          "&nbsp;&nbsp;&nbsp;&nbsp;datatypes. Double-click to<br>",
          "&nbsp;&nbsp;&nbsp;&nbsp;isolate.<br>",
          "&nbsp;&nbsp;• hover over bars/points<br>",
          "&nbsp;&nbsp;&nbsp;&nbsp;for details"
        ),
        valign = "top",
        showarrow = FALSE,
        xanchor = "left", yanchor = "top", align = "left",
        xshift = 10, yshift = 0,
        font = list(size = 11, color = "black")
      )
    )) %>%
    config(doubleClick = "reset", displaylogo = FALSE, displayModeBar = FALSE,
      modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d", "autoScale2d"),
      toImageButtonOptions = list(
        filename = str_c(format(Sys.time(), "%Y%m%d"), "-rEffEstimationPlot"),
        format = "png",
        width = 1000,
        height = 1000))

  plot$elementId <- widgetID

  return(plot)
}
