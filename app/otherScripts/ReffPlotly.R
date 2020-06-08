# TO-DO: deduplicate code & general clean up...

toLowerFirst <- function(string) {
  str_replace(string, ".{1}", tolower(str_extract(string, ".{1}")))
}

rEffPlotly <- function(
  caseData,
  estimates,
  interventions,
  plotColors,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date),
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  legendOrientation = "v", # "v" or "h"
  language,
  translator,
  widgetID = "rEffplots") {

  # plot parameter
  dateFormat <- translator$t("%b-%d")
  dateFormatLong <- translator$t("%Y-%m-%d")

  if (language %in% c("de-ch", "fr-ch")) {
    locale <- language
  } else if (language == "en-gb") {
    locale <- NULL
  } else if (language == "it-ch") {
    locale <- "it"
  }

  names(plotColors) <- sapply(names(plotColors), translator$t,  USE.NAMES = FALSE)

  if (legendOrientation == "v") {
    xrNote <- 1
    yrNote <- 0.35
    rNote <- translator$t(str_c(
      "<b>*</b>&nbsp;This is the most recent<br>possible R<sub>e</sub> estimate due to <br>",
      "delays between infection and<br>",
      "the last data observation."))
    rNoteAnchors <- c("left", "bottom")
    xHelpBox <- 1
    yHelpBox <- 0.72
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 174
    hHelpBox <- 90
    if (language %in% c("fr-ch", "it-ch")) {
      hHelpBox <- 120
    } else if (language == "de-ch") {
      hHelpBox <- 130
    }
    helpBoxText <- translator$t(str_c(
      "&nbsp;<b>Interactive plot</b><br>",
      "&nbsp;&nbsp;• Click on legend toggles<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;datatypes; doubleclick<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;isolates datatypes.<br>",
      "&nbsp;&nbsp;• Hovering the mouse over<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;data points shows details."
    ))
    helpBoxShift <- c(10, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80
  } else if (legendOrientation == "h") {
    xrNote <- 0.99
    yrNote <- 0.34
    rNote <- translator$t(str_c(
      "<b>*</b>&nbsp;This is the most recent<br>possible R<sub>e</sub> estimate due to <br>",
      "delays between infection and <br>",
      "the last data observation."))
    rNoteAnchors <- c("right", "bottom")
    xHelpBox <- 0
    yHelpBox <- -0.15
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 550
    hHelpBox <- 30
    helpBoxText <- translator$t(str_c(
      "&nbsp;<b>Interactive plot</b><br>",
      "&nbsp;&nbsp;• Click on legend toggles<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;datatypes; doubleclick<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;isolates datatypes.<br>",
      "&nbsp;&nbsp;• Hovering the mouse over<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;data points shows details."
    ))
    helpBoxShift <- c(0, 0)
    xDataSource <- 0
    yDataSource <- -0.21
    dataSourceAnchors <- c("left", "top")
    bottomMargin <- 140
  } else {
    stop("legendOrientation must be either \"v\" or \"h\".")
  }

  # prepare Data
  newLevels <- levels(caseData$data_type)
  names(newLevels) <- sapply(newLevels, translator$t,  USE.NAMES = FALSE)

  caseData <- caseData %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels))

  estimatesPlot <- estimates %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels))

  pCases <- plot_ly(data = caseData) %>%
    add_bars(x = ~date, y = ~incidence, color = ~data_type,
      colors = plotColors,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}",
      legendgroup = ~data_type) %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[1]),
      yaxis = plotlyYaxis(
        title = translator$t("New observations"),
        fixedRange = fixedRangeY[1]),
      legend = list(title = list(text = str_c("<b>", translator$t("Data types"), "</b>")))
      )

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColors,
      type = "scatter", mode = "lines",
      legendgroup = ~data_type, showlegend = FALSE,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", data_type, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~data_type, colors = plotColors,
      line = list(color = "transparent"), opacity = 0.5,
      legendgroup = ~data_type, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(data_type) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~data_type, colors = plotColors,
      legendgroup = ~data_type,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", data_type, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(
        translator$t("Exponential increase<br>in number of new cases"),
        translator$t("Decrease in number of new cases")),
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
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[2]),
      yaxis = plotlyYaxis(
        title = translator$t("Reproductive number R<sub>e</sub>"),
        range = c(0, 2),
        fixedRange = fixedRangeY[2],
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", translator$t("Data types"), "</b>"))
      ),
      shapes = list(
        list(
          type = "line",
          x0 = startDate, x1 = endDate,
          y0 = 1, y1 = 1,
          line = list(color = "red", width = 0.5)
        )
      )
    )

  if (!is.null(interventions)) {
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
        xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[3]),
        yaxis = plotlyYaxis(visible = FALSE, fixedRange = fixedRangeY[3]))
    plotlist <- list(pCases, pEstimates, pIntervention)
    nPlots <- 3
  } else {
    plotlist <- list(pCases, pEstimates)
    nPlots <- 2
  }

  plot <- subplot(plotlist, nrows = nPlots, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
    layout(
      margin = list(b = bottomMargin),
      legend = list(orientation = legendOrientation),
      annotations = list(
        list(
          x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate, name = translator$t("Data Source"), dateFormatLong),
          showarrow = FALSE,
          xanchor = dataSourceAnchors[1], yanchor = dataSourceAnchors[2], xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")),
        list(
          x = xrNote, y = yrNote, xref = "paper", yref = "paper",
          text = rNote,
          showarrow = FALSE,
          xanchor = rNoteAnchors[1], yanchor = rNoteAnchors[2], align = "left",
          xshift = 10, yshift = 0,
          font = list(size = 11, color = "black")),
        list(
          x = xHelpBox, y = yHelpBox, xref = "paper", yref = "paper",
          width = wHelpBox,
          height = hHelpBox,
          bgcolor = "#eeeeee",
          text = helpBoxText,
          valign = "top",
          showarrow = FALSE,
          xanchor = helpBoxAnchors[1], yanchor = helpBoxAnchors[2], align = "left",
          xshift = helpBoxShift[1], yshift = helpBoxShift[2],
          font = list(size = 11, color = "black")
        )
    )) %>%
    config(doubleClick = "reset", displaylogo = FALSE, displayModeBar = FALSE,
      locale = locale, scrollZoom = TRUE)

  plot$elementId <- widgetID

  return(plot)
}

rEffPlotlyRegion <- function(
  caseData,
  estimates,
  interventions,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date),
  legendOrientation = "v", # "v" or "h"
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  regionColors,
  translator,
  language,
  widgetID = "rEffplotsRegions",
  visibilityNonFocus = "legendonly"
  ) {

  # plot parameter
  dateFormat <- translator$t("%b-%d")
  dateFormatLong <- translator$t("%Y-%m-%d")

  if (language %in% c("de-ch", "fr-ch")) {
    locale <- language
  } else if (language == "en-gb") {
    locale <- NULL
  } else if (language == "it-ch") {
    locale <- "it"
  }

  axisTitleFontSize <- 14
  if (legendOrientation == "v") {
    xrNote <- 0.99
    yrNote <- 0.60
    rNote <- translator$t(str_c(
      "<b>*</b>&nbsp;This is the most recent<br>possible R<sub>e</sub> estimate due to <br>",
      "delays between infection and<br>",
      "the last data observation."))
    rNoteAnchors <- c("right", "top")
    xHelpBox <- 1
    yHelpBox <- 0
    helpBoxAnchors <- c("left", "bottom")
    wHelpBox <- 174
    hHelpBox <- 90
    if (language %in% c("fr-ch", "it-ch")) {
      hHelpBox <- 120
    } else if (language == "de-ch") {
      hHelpBox <- 130
    }
    helpBoxText <- translator$t(str_c(
      "&nbsp;<b>Interactive plot</b><br>",
      "&nbsp;&nbsp;• Click on legend toggles<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;datatypes; doubleclick<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;isolates datatypes.<br>",
      "&nbsp;&nbsp;• Hovering the mouse over<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;data points shows details."
    ))
    helpBoxShift <- c(10, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80
    rightMargin <- 200
  } else if (legendOrientation == "h") {
    xrNote <- 0.99
    yrNote <- 0.60
    rNote <- translator$t(str_c(
      "<b>*</b>&nbsp;This is the most recent<br>possible R<sub>e</sub> estimate due to <br>",
      "delays between infection and <br>",
      "the last data observation."))
    rNoteAnchors <- c("right", "top")
    xHelpBox <- 0
    yHelpBox <- -0.1
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 500
    hHelpBox <- 50
    helpBoxText <- translator$t(str_c(
      "&nbsp;<b>Interactive plot</b><br>",
      "&nbsp;&nbsp;• Click on legend toggles<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;datatypes; doubleclick<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;isolates datatypes.<br>",
      "&nbsp;&nbsp;• Hovering the mouse over<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;data points shows details."
    ))
    helpBoxShift <- c(0, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "top")
    bottomMargin <- 125
    rightMargin <- 200
  } else {
    stop("legendOrientation must be either \"v\" or \"h\".")
  }

  # prepare Data
  newLevels <- levels(caseData$data_type)
  names(newLevels) <- sapply(newLevels, translator$t,  USE.NAMES = FALSE)

  names(regionColors) <- recode(
    names(regionColors),
    Switzerland = translator$t("Switzerland (Total)"))

  caseData <- caseData %>%
    filter(data_type == "Confirmed cases") %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, Switzerland = translator$t("Switzerland (Total)")))

  caseDataCH <- filter(caseData, region == translator$t("Switzerland (Total)"))

  estimatesPlot <- estimates %>%
    filter(data_type == "Confirmed cases") %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, Switzerland = translator$t("Switzerland (Total)")))

  estimatesPlotCH <- filter(estimatesPlot, region == translator$t("Switzerland (Total)"))
  
  pCases <- plot_ly(data = caseData) %>%
    filter(region != translator$t("Switzerland (Total)")) %>%
    add_bars(x = ~date, y = ~incidence, color = ~region, colors = regionColors,
      legendgroup = ~region, visible = visibilityNonFocus,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_bars(data = caseDataCH, x = ~date, y = ~incidence, color = ~region, colors = regionColors,
      legendgroup = ~region,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[1]),
      yaxis = plotlyYaxis(
        title = translator$t("New observations"),
        fixedRange = fixedRangeY[1]),
      legend = list(title = list(text = str_c("<b>", translator$t("Data types"), "</b>")))
    )

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    filter(region != translator$t("Switzerland (Total)")) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~region, colors = regionColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~region, visible = visibilityNonFocus,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~region, legendgroup = ~region, visible = visibilityNonFocus,
      line = list(color = "transparent"), opacity = 0.5, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(region) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers", visible = visibilityNonFocus,
      color = ~region, colors = regionColors,
      legendgroup = ~region,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_trace(
      data = estimatesPlotCH,
      x = ~date, y = ~median_R_mean, color = ~region, colors = regionColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~region,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      data = estimatesPlotCH,
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~region, legendgroup = ~region,
      line = list(color = "transparent"), opacity = 0.5, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(region) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~region, colors = regionColors,
      legendgroup = ~region,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(
        translator$t("Exponential increase<br>in number of new cases"),
        translator$t("Decrease in number of new cases")),
      font = list(color = "red"),
      x = startDate,
      y = c(1.30, 0.85),
      textangle = 0,
      align = "left",
      xanchor = "left",
      yanchor = "middle",
      showarrow = FALSE,
      inherit = FALSE) %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[2]),
      yaxis = plotlyYaxis(
        title = translator$t("Reproductive number R<sub>e</sub>"),
        range = c(0, 2),
        fixedRange = fixedRangeY[2],
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", translator$t("Canton"), "</b>"))
      ),
      shapes = list(
        list(
          type = "line",
          x0 = startDate, x1 = endDate,
          y0 = 1, y1 = 1,
          line = list(color = "red", width = 0.5)
        )
      )
    )
  if (!is.null(interventions)) {
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
        xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[3]),
        yaxis = plotlyYaxis(visible = FALSE, fixedRange = fixedRangeY[3]))
    plotlist <- list(pCases, pEstimates, pIntervention)
    nPlots <- 3
  } else {
    plotlist <- list(pCases, pEstimates)
    nPlots <- 2
    yrNote <- 0.40
  }

  plot <- subplot(plotlist, nrows = nPlots, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
    layout(
      margin = list(b = bottomMargin, r = rightMargin),
      legend = list(orientation = "v"),
      annotations = list(
        list(
          x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate, name = translator$t("Data Source"), dateFormatLong),
          showarrow = FALSE,
          xanchor = dataSourceAnchors[1], yanchor = dataSourceAnchors[2], xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")),
        list(
          x = xrNote, y = yrNote, xref = "paper", yref = "paper",
          text = rNote,
          showarrow = FALSE,
          xanchor = rNoteAnchors[1], yanchor = rNoteAnchors[2], align = "left",
          xshift = 10, yshift = 0,
          font = list(size = 11, color = "black")),
        list(
          x = xHelpBox, y = yHelpBox, xref = "paper", yref = "paper",
          width = wHelpBox,
          height = hHelpBox,
          bgcolor = "#eeeeee",
          text = helpBoxText,
          valign = "top",
          showarrow = FALSE,
          xanchor = helpBoxAnchors[1], yanchor = helpBoxAnchors[2], align = "left",
          xshift = helpBoxShift[1], yshift = helpBoxShift[2],
          font = list(size = 11, color = "black")
        )
    )) %>%
    config(doubleClick = "reset", displaylogo = FALSE, displayModeBar = FALSE,
      locale = locale, scrollZoom = TRUE)

  plot$elementId <- widgetID

  return(plot)
}

rEffPlotlyComparison <- function(
  caseData,
  estimates,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date),
  focusCountry = "Switzerland",
  legendOrientation = "v", # "v" or "h"
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  countryColors,
  translator,
  language,
  widgetID = "rEffplotsComparison") {

  # plot parameter
  dateFormat <- translator$t("%b-%d")
  dateFormatLong <- translator$t("%Y-%m-%d")

  if (language %in% c("de-ch", "fr-ch")) {
    locale <- language
  } else if (language == "en-gb") {
    locale <- NULL
  } else if (language == "it-ch") {
    locale <- "it"
  }

  axisTitleFontSize <- 14
  if (legendOrientation == "v") {
    xrNote <- 0.99
    yrNote <- 0.35
    rNote <- translator$t(str_c(
      "<b>*</b>&nbsp;This is the most recent<br>possible R<sub>e</sub> estimate due to <br>",
      "delays between infection and<br>",
      "the last data observation."))
    rNoteAnchors <- c("right", "top")
    xHelpBox <- 1
    yHelpBox <- 0.2
    helpBoxAnchors <- c("left", "bottom")
    wHelpBox <- 174
    hHelpBox <- 90
    if (language %in% c("fr-ch", "it-ch")) {
      hHelpBox <- 120
    } else if (language == "de-ch") {
      hHelpBox <- 130
    }
    helpBoxText <- translator$t(str_c(
      "&nbsp;<b>Interactive plot</b><br>",
      "&nbsp;&nbsp;• Click on legend toggles<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;datatypes; doubleclick<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;isolates datatypes.<br>",
      "&nbsp;&nbsp;• Hovering the mouse over<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;data points shows details."
    ))
    helpBoxShift <- c(10, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80
    rightMargin <- 200
  } else if (legendOrientation == "h") {
    xrNote <- 0.99
    yrNote <- 0.60
    rNote <- translator$t(str_c(
      "<b>*</b>&nbsp;This is the most recent<br>possible R<sub>e</sub> estimate due to <br>",
      "delays between infection and <br>",
      "the last data observation."))
    rNoteAnchors <- c("right", "top")
    xHelpBox <- 0
    yHelpBox <- -0.2
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 400
    hHelpBox <- 50
    helpBoxText <- translator$t(str_c(
      "&nbsp;<b>Interactive plot</b><br>",
      "&nbsp;&nbsp;• Click on legend toggles<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;datatypes; doubleclick<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;isolates datatypes.<br>",
      "&nbsp;&nbsp;• Hovering the mouse over<br>",
      "&nbsp;&nbsp;&nbsp;&nbsp;data points shows details."
    ))
    helpBoxShift <- c(0, 0)
    xDataSource <- 0
    yDataSource <- -0.21
    dataSourceAnchors <- c("left", "top")
    bottomMargin <- 200
  } else {
    stop("legendOrientation must be either \"v\" or \"h\".")
  }

  # prepare Data

  caseDataFocus <- filter(caseData, country == focusCountry)

  estimatesPlot <- estimates
  estimatesPlotFocus <- filter(estimatesPlot, country == focusCountry)

  pCases <- plot_ly(data = caseData) %>%
    filter(country != focusCountry) %>%
    add_bars(x = ~date, y = ~incidence, color = ~country, colors = countryColors,
      legendgroup = ~country, visible = "legendonly",
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_bars(data = caseDataFocus, x = ~date, y = ~incidence, color = ~country, colors = countryColors,
      legendgroup = ~country,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[1]),
      yaxis = plotlyYaxis(
        title = translator$t("New observations"),
        fixedRange = fixedRangeY[1]),
      legend = list(title = list(text = "<b> Data Type </b>")))

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    filter(country != focusCountry) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~country, colors = countryColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~country, visible = "legendonly",
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", country, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~country, legendgroup = ~country, visible = "legendonly",
      line = list(color = "transparent"), opacity = 0.5, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(country) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers", visible = "legendonly",
      color = ~country, colors = countryColors,
      legendgroup = ~country,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", country, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_trace(
      data = estimatesPlotFocus,
      x = ~date, y = ~median_R_mean, color = ~country, colors = countryColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~country,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", country, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      data = estimatesPlotFocus,
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~country, legendgroup = ~country,
      line = list(color = "transparent"), opacity = 0.5, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(country) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~country, colors = countryColors,
      legendgroup = ~country,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(
        translator$t("Exponential increase<br>in number of new cases"),
        translator$t("Decrease in number of new cases")),
      font = list(color = "red"),
      x = startDate,
      y = c(1.30, 0.85),
      textangle = 0,
      align = "left",
      xanchor = "left",
      yanchor = "middle",
      showarrow = FALSE,
      inherit = FALSE) %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[2]),
      yaxis = plotlyYaxis(
        title = translator$t("Reproductive number R<sub>e</sub>"),
        range = c(0, 2),
        fixedRange = fixedRangeY[2],
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", translator$t("Country"), "</b>"))
      ),
      shapes = list(
        list(
          type = "line",
          x0 = startDate, x1 = endDate,
          y0 = 1, y1 = 1,
          line = list(color = "red", width = 0.5)
        )
      )
    )

  plotlist <- list(pCases, pEstimates)
  plot <- subplot(plotlist, nrows = 2, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
    layout(
      margin = list(b = bottomMargin, r = rightMargin),
      legend = list(orientation = legendOrientation),
      annotations = list(
        list(
          x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate, name = translator$t("Data Source"), dateFormatLong),
          showarrow = FALSE,
          xanchor = dataSourceAnchors[1], yanchor = dataSourceAnchors[2], xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")),
        list(
          x = xrNote, y = yrNote, xref = "paper", yref = "paper",
          text = rNote,
          showarrow = FALSE,
          xanchor = rNoteAnchors[1], yanchor = rNoteAnchors[2], align = "left",
          xshift = 10, yshift = 0,
          font = list(size = 11, color = "black")),
        list(
          x = xHelpBox, y = yHelpBox, xref = "paper", yref = "paper",
          width = wHelpBox,
          height = hHelpBox,
          bgcolor = "#eeeeee",
          text = helpBoxText,
          valign = "top",
          showarrow = FALSE,
          xanchor = helpBoxAnchors[1], yanchor = helpBoxAnchors[2], align = "left",
          xshift = helpBoxShift[1], yshift = helpBoxShift[2],
          font = list(size = 11, color = "black")
        )
    )) %>%
    config(doubleClick = "reset", displaylogo = FALSE, displayModeBar = FALSE,
      locale = locale, scrollZoom = TRUE)

  plot$elementId <- widgetID

  return(plot)
}
