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
  endDate = max(caseData$date) + 1,
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  logCaseYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
  popSizes = NULL,
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

  # layout pars
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
    yDataSource <- -0.2
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 0

  # prepare Data
  newLevels <- levels(caseData$data_type)
  names(newLevels) <- sapply(newLevels, translator$t,  USE.NAMES = FALSE)

  caseData <- caseData %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels))

  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      left_join(popSizes, by = c("country", "region")) %>%
      mutate(incidence = incidence / popSize * 100000)
    pCasesTitle <- str_c(pCasesTitle, " / 100'000")
  }

  if (caseAverage > 1) {
    caseData <- caseData %>%
      group_by(data_type) %>%
      mutate(
        incidence = slide_index_dbl(incidence, date, mean, .before = lubridate::days(caseAverage))
      ) %>%
      ungroup()
    pCasesTitle <- str_c(pCasesTitle, "\n(", translator$t("7 day avarage"), ")")
  }

  if (logCaseYaxis) {
    zoomRange <- makeZoomRange(log10(max(caseData$incidence, na.rm = TRUE)), extra = log10(5), stepSize = log10(10))
  } else {
    zoomRange <- makeZoomRange(max(caseData$incidence, na.rm = TRUE))
  }

  estimatesPlot <- estimates %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels))

  pCases <- plot_ly(data = caseData) %>%
    add_bars(x = ~date, y = ~incidence, color = ~data_type,
      colors = plotColors,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(incidence, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
        "<extra></extra>"),
      hovertemplate = "%{text}",
      legendgroup = ~data_type) %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[1], rSlider = FALSE, rSelector = TRUE),
      yaxis = plotlyYaxis(
        title = pCasesTitle,
        fixedRange = fixedRangeY[1],
        logAxis = logCaseYaxis),
      legend = list(title = list(text = str_c("<b>", translator$t("Data types"), "</b>"))),
      sliders = list(
        makeSlider(zoomRange)
      )
    )

  if (caseLoess) {
    pCases <- pCases %>%
      add_trace(x = ~date, y = ~incidenceLoess, color = ~data_type, color = plotColors,
      type = "scatter", mode = "lines", opacity = 0.5,
      text = ~str_c("<i> Loess Fit </i><extra></extra>"),
      legendgroup = ~data_type, showlegend = FALSE,
      hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      filter(!is.na(deconvoluted)) %>%
      add_trace(x = ~date, y = ~deconvoluted, color = ~data_type, color = plotColors,
        type = "scatter", mode = "lines",
        text = ~str_c("<i> deconvoluted Data </i><extra></extra>"),
        legendgroup = ~data_type, showlegend = FALSE,
        hovertemplate = "%{text}") %>%
      add_ribbons(
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~data_type, colors = plotColors,
        opacity = 0.2,
        legendgroup = ~data_type, showlegend = FALSE,
        hoverinfo = "none")
  }

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColors,
      type = "scatter", mode = "lines",
      legendgroup = ~data_type, showlegend = FALSE,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", data_type, ")<extra></extra>"),
      hovertemplate = "%{text}",
      showlegend = FALSE) %>%
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
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", data_type, ")"),
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
        range = c(0, 4),
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
      locale = locale, scrollZoom = FALSE)

  plot$elementId <- widgetID

  return(plot)
}

rEffPlotlyRegion <- function(
  caseData,
  estimates,
  interventions,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date) + 1,
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  logCaseYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
  popSizes = NULL,
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

  # layout pars
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
    yDataSource <- -0.2
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 0
    rightMargin <- 200

  # prepare Data
  newLevels <- levels(caseData$data_type)
  names(newLevels) <- sapply(newLevels, translator$t,  USE.NAMES = FALSE)

  names(regionColors) <- recode(
    names(regionColors),
    Switzerland = translator$t("Switzerland (Total)"))

  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      left_join(popSizes, by = c("country", "region")) %>%
      mutate(incidence = incidence / popSize * 100000)
    pCasesTitle <- str_c(pCasesTitle, " / 100'000")
  }

  caseData <- caseData %>%
    filter(data_type == "Confirmed cases") %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, Switzerland = translator$t("Switzerland (Total)")))

  if (caseAverage > 1) {
    caseData <- caseData %>%
      group_by(data_type, country, region) %>%
      mutate(
        incidence = slide_index_dbl(incidence, date, mean, .before = lubridate::days(caseAverage))
      ) %>%
      ungroup()
    pCasesTitle <- str_c(pCasesTitle, "\n(", translator$t("7 day avarage"), ")")
  }

  caseDataCH <- filter(caseData, region == translator$t("Switzerland (Total)"))

  estimatesPlot <- estimates %>%
    filter(data_type == "Confirmed cases") %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, Switzerland = translator$t("Switzerland (Total)")))

  estimatesPlotCH <- filter(estimatesPlot, region == translator$t("Switzerland (Total)"))

  if (logCaseYaxis) {
    zoomRange <- makeZoomRange(log10(max(caseData$incidence, na.rm = TRUE)), extra = log10(5), stepSize = log10(10))
  } else {
    zoomRange <- makeZoomRange(max(caseData$incidence, na.rm = TRUE))
  }

  pCases <- plot_ly(data = caseData) %>%
    filter(region != translator$t("Switzerland (Total)")) %>%
    add_bars(x = ~date, y = ~incidence, color = ~region, colors = regionColors,
      legendgroup = ~region, visible = visibilityNonFocus,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(incidence, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
        "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_bars(data = caseDataCH, x = ~date, y = ~incidence, color = ~region, colors = regionColors,
      legendgroup = ~region,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(incidence, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
        "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[1], rSlider = FALSE, rSelector = TRUE),
      yaxis = plotlyYaxis(
        title = pCasesTitle,
        fixedRange = fixedRangeY[1],
        logAxis = logCaseYaxis),
      legend = list(title = list(text = str_c("<b>", translator$t("Data types"), "</b>"))),
      sliders = list(
        makeSlider(zoomRange)
      )
    )

  if (caseLoess) {
    pCases <- pCases %>%
      add_trace(data = filter(caseData, region != translator$t("Switzerland (Total)")),
        x = ~date, y = ~incidenceLoess, color = ~region, color = regionColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~region, showlegend = FALSE, visible = "legendonly",
        hovertemplate = "%{text}") %>%
      add_trace(data = caseDataCH,
        x = ~date, y = ~incidenceLoess, color = ~region, color = regionColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~region, showlegend = FALSE,
        hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      add_trace(
        data = filter(caseData, region != translator$t("Switzerland (Total)"), !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~region, color = regionColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> deconvoluted Data </i><extra></extra>"),
        legendgroup = ~region, showlegend = FALSE, visible = "legendonly",
        hovertemplate = "%{text}") %>%
      add_ribbons(
        data =  filter(caseData, region != translator$t("Switzerland (Total)"), !is.na(deconvoluted)),
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~region, colors = regionColors,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~region, showlegend = FALSE, visible = "legendonly",
        hoverinfo = "none") %>%
      add_trace(
        data = filter(caseDataCH, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~region, color = regionColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> deconvoluted Data </i><extra></extra>"),
        legendgroup = ~region, showlegend = FALSE,
        hovertemplate = "%{text}") %>%
      add_ribbons(
        data =  filter(caseDataCH, !is.na(deconvoluted)),
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~region, colors = regionColors,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~region, showlegend = FALSE,
        hoverinfo = "none")
  }

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    filter(region != translator$t("Switzerland (Total)")) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~region, colors = regionColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~region, visible = visibilityNonFocus,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
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
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", region, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_trace(
      data = estimatesPlotCH,
      x = ~date, y = ~median_R_mean, color = ~region, colors = regionColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~region,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
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
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
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
        range = c(0, 4),
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
      locale = locale, scrollZoom = FALSE)

  plot$elementId <- widgetID

  return(plot)
}

rEffPlotlyComparison <- function(
  caseData,
  estimates,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date) + 1,
  focusCountry = "Switzerland",
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  logCaseYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
  popSizes = NULL,
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

  # layout pars
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
    yDataSource <- -0.18
    dataSourceAnchors <- c("right", "top")
    bottomMargin <- 0
    rightMargin <- 200

  # prepare Data
  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      left_join(popSizes, by = c("country", "region")) %>%
      mutate(incidence = incidence / popSize * 100000)
    pCasesTitle <- str_c(pCasesTitle, " / 100'000")
  }

  if (caseAverage > 1) {
    caseData <- caseData %>%
      group_by(country, region, data_type) %>%
      mutate(
        incidence = slide_index_dbl(incidence, date, mean, .before = lubridate::days(caseAverage))
      ) %>%
      ungroup()
    pCasesTitle <- str_c(pCasesTitle, "\n(", translator$t("7 day avarage"), ")")
  }
  caseDataFocus <- filter(caseData, country == focusCountry)

  estimatesPlot <- estimates
  estimatesPlotFocus <- filter(estimatesPlot, country == focusCountry)

  if (logCaseYaxis) {
    zoomRange <- makeZoomRange(log10(max(caseData$incidence, na.rm = TRUE)), extra = log10(5), stepSize = log10(10))
  } else {
    zoomRange <- makeZoomRange(max(caseData$incidence, na.rm = TRUE))
  }

  pCases <- plot_ly(data = caseData) %>%
    filter(country != focusCountry) %>%
    add_bars(x = ~date, y = ~incidence, color = ~country, colors = countryColors,
      legendgroup = ~country, visible = "legendonly",
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(incidence, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
        "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_bars(data = caseDataFocus, x = ~date, y = ~incidence, color = ~country, colors = countryColors,
      legendgroup = ~country,
      text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(incidence, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
        "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX[1], rSlider = FALSE,  rSelector = TRUE),
      yaxis = plotlyYaxis(
        title = pCasesTitle,
        fixedRange = fixedRangeY[1],
        logAxis = logCaseYaxis),
      legend = list(title = list(text = "<b> Data Type </b>")),
      sliders = list(
        makeSlider(zoomRange)
      )
    )

  if (caseLoess) {
    pCases <- pCases %>%
      add_trace(data = filter(caseData, country != focusCountry),
        x = ~date, y = ~incidenceLoess, color = ~country, color = countryColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~country, showlegend = FALSE, visible = "legendonly",
        hovertemplate = "%{text}") %>%
      add_trace(data = caseDataFocus,
        x = ~date, y = ~incidenceLoess, color = ~country, color = countryColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~country, showlegend = FALSE,
        hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      add_trace(
        data = filter(caseData, country != focusCountry, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~country, color = countryColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> deconvoluted Data </i><extra></extra>"),
        legendgroup = ~country, showlegend = FALSE, visible = "legendonly",
        hovertemplate = "%{text}") %>%
      add_ribbons(
        data =  filter(caseData, country != focusCountry, !is.na(deconvoluted)),
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~country, colors = countryColors,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~country, showlegend = FALSE, visible = "legendonly",
        hoverinfo = "none") %>%
      add_trace(
        data = filter(caseDataFocus, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~country, color = countryColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> deconvoluted Data </i><extra></extra>"),
        legendgroup = ~country, showlegend = FALSE,
        hovertemplate = "%{text}") %>%
      add_ribbons(
        data =  filter(caseDataFocus, !is.na(deconvoluted)),
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~country, colors = countryColors,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~country, showlegend = FALSE,
        hoverinfo = "none")
  }

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    filter(country != focusCountry) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~country, colors = countryColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~country, visible = "legendonly",
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
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
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", country, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_trace(
      data = estimatesPlotFocus,
      x = ~date, y = ~median_R_mean, color = ~country, colors = countryColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~country,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
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
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
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
        range = c(0, 4),
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
      locale = locale, scrollZoom = FALSE)

  plot$elementId <- widgetID

  return(plot)
}

plotlyXaxis <- function(startDate, endDate, dateFormat, fixedRange, rSlider = FALSE, rSelector = FALSE) {
    out <- list(
      title = "",
      type = "date",
      range = c(startDate, endDate),
      #tickvals = seq(startDate, endDate, length.out = 18),
      tickformat = dateFormat,
      tickangle = 45,
      showgrid = TRUE,
      fixedrange = fixedRange
    )
    if (rSlider) {
      out$rangeslider <- list(
        type = "date",
        range = c(startDate, endDate),
        thickness = 0.05,
        yaxis2 = list(range = c(-2, -1)),
        yaxis3 = list(range = c(-2, -1)))
    }
    if (rSelector) {
      out$rangeselector <- list(
        buttons = list(
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backwards"),
          list(
            count = 2,
            label = "2 mo",
            step = "month",
            stepmode = "backwards"),
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backwards"),
          list(
            count = as.integer(difftime(endDate, startDate)),
            step = "day",
            label = "all",
            stepmode = "backwards")))
    }
    return(out)
}

plotlyYaxis <- function(
    title = "",
    range = NULL, fixedRange = TRUE,
    zeroline = TRUE,
    visible = TRUE,
    logAxis = FALSE,
    axisTitleFontSize = 14
    ) {
  out <- list(
    visible = visible,
    range = range,
    fixedrange = fixedRange,
    zeroline = zeroline,
    title = list(
      text = title,
      font = list(size = axisTitleFontSize)))

  if (logAxis) {
    out$type <- "log"
    out$dtick <- 1
  }

  return(out)
}

makeZoomRange <- function(maxZoom, stepSize = 10, extra = 5) {
  zoomLevels <- rev(seq(0, maxZoom + extra * stepSize, by = stepSize)[-1])
  zoomRange <- list()
  for (i in seq_along(zoomLevels)) {
    zoomRange[[i]] <- list(
      method = "relayout",
      args = list(list(yaxis.range = c(0, zoomLevels[i]))),
      label = "")
  }
  return(zoomRange)
}

makeSlider <- function(zoomRange, x = 0.01, y = 1, anchor = c("top", "left")) {
  slider <- list(
    active = 0,
    currentvalue = list(prefix = "<b>Zoom</b>"),
    pad = list(t = 0, l = 0, r = 0, b = 0),
    steps = zoomRange,
    len = 0.20,
    x = x,
    xanchor = anchor[1],
    y = y,
    yanchor = anchor[2],
    ticklen = 0, minorticklen = 0
  )
  return(slider)
}
