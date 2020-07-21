# TO-DO: deduplicate code & general clean up...
library(viridisLite)
library(shades)

# colors
allCols <- viridis(6)
plotColors <-  c(
  "Confirmed cases" = allCols[1],
  "Confirmed cases / tests" = allCols[2],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5],
  "Excess deaths" = allCols[6])
plotColorsTruncated <- saturation(plotColors, value = 0.1)
names(plotColorsTruncated) <- str_c(names(plotColors), " truncated")
plotColorsTruncated[1] <- "#aba3ad"
plotColors <- c(plotColors, plotColorsTruncated)

fixedRangeX <- c(FALSE, FALSE, FALSE)
fixedRangeY <- c(TRUE, TRUE, TRUE)


# functions
rEffPlotly <- function(
  caseData,
  estimates,
  interventions,
  plotColors,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date) + 1,
  caseDataRightTruncation = 2,
  fixedRangeX = c(TRUE, TRUE, TRUE),
  fixedRangeY = c(TRUE, TRUE, TRUE),
  logCaseYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
  showTraces = NULL,
  showTracesMode = "only",
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
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80

  # prepare Data
  if (!("testPositivity" %in% colnames(caseData))) {
    caseData$testPositivity <- NA
    caseData$totalTests <- NA
    caseData$positiveTests <- NA
    caseData$negativeTests <- NA
  }

  newLevels <- levels(caseData$data_type)
  names(newLevels) <- sapply(newLevels, translator$t,  USE.NAMES = FALSE)

  caseData <- caseData %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels)) %>%
    group_by(data_type) %>%
    mutate(
      tooltipText = str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(incidence, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
        if_else(data_type == "Confirmed cases" & !is.na(testPositivity),
          str_c("<br>Test positivity ", round(testPositivity, 3), " (", positiveTests, " / ", negativeTests, ")"),
          ""
        ),
        if_else(data_type == "Confirmed cases / tests",
          str_c("<br>", incidence * totalTests, " cases",
            "<br>Test positivity ", round(testPositivity, 3), " (", positiveTests, " / ", negativeTests, ")"
          ),
          ""
        ))
    )

  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      mutate(incidence = incidence / populationSize * 100000)
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

  if (caseDataRightTruncation > 0) {
    caseDataTrunc <- caseData %>%
      group_by(data_type) %>%
      filter(date <= max(date) - caseDataRightTruncation)
    caseDataRest <- caseData %>%
      group_by(data_type) %>%
      filter(date > max(date) - caseDataRightTruncation) %>%
      mutate(data_type_plot = str_c(data_type, " truncated"))
  } else {
    caseDataTrunc <- caseData
  }

  pCases <- plot_ly(data = caseDataTrunc) %>%
    add_bars(x = ~date, y = ~incidence, color = ~data_type,
      colors = plotColors,
      text = ~str_c(tooltipText, "<extra></extra>"),
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

  if (caseDataRightTruncation > 0) {
    pCases <- pCases %>%
      add_bars(
        data = caseDataRest,
        x = ~date, y = ~incidence, color = ~data_type_plot,
        colors = plotColors,
        text = ~str_c(tooltipText, "<br>(not used for R<sub>e</sub> estimates)<extra></extra>"),
        hovertemplate = "%{text}", inherit = FALSE,
        legendgroup = ~data_type, showlegend = FALSE)
  }

  if (caseLoess) {
    caseDataTruncLoess <- caseDataTrunc %>%
      filter(data_type != "Excess deaths") %>%
      group_by(country, region, source, data_type) %>%
      mutate(incidenceLoess = getLOESSCases(date, incidence))
    pCases <- pCases %>%
      add_trace(
        data = caseDataTruncLoess,
        x = ~date, y = ~incidenceLoess, color = ~data_type, color = plotColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~data_type, showlegend = FALSE,
        hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      add_trace(
        data = filter(caseData, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~data_type, color = plotColors,
        type = "scatter", mode = "lines",
        text = ~str_c("<i> deconvoluted Data +/- sd.</i><extra></extra>"),
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
    config(doubleClick = "reset", displaylogo = FALSE, modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1, filename = "ReEstimates"),
      locale = locale, scrollZoom = FALSE)

  plot$elementId <- widgetID

  if (!is.null(showTraces)) {
    plot <- plotlyShowTraces(plot, showTraces, mode = showTracesMode)
  }

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
  caseDataRightTruncation = 2,
  logCaseYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
  regionTitle = "Canton",
  regionColors,
  translator,
  language,
  widgetID = "rEffplotsRegions",
  focusRegion = NULL,
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
    xrNote <- 0.90
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
    bottomMargin <- 80
    rightMargin <- 200

  # prepare Data
  newLevels <- levels(caseData$data_type)
  names(newLevels) <- sapply(newLevels, translator$t,  USE.NAMES = FALSE)

  if (!is.null(focusRegion)) {
    countryNames <- caseData %>% ungroup() %>% select(countryIso3, country) %>% distinct()
    focusRegionLong <- str_c(countryNames$country[countryNames$countryIso3 == focusRegion], " (Total)")
    focusRegionRecodeKey <- c(
      translator$t(focusRegionLong),
      translator$t(str_c(focusRegionLong, " truncated"))
    )
    names(focusRegionRecodeKey) <- c(focusRegion, str_c(focusRegion, " truncated"))

    names(regionColors) <- recode(
      names(regionColors),
      !!!focusRegionRecodeKey)
  } else {
    focusRegionRecodeKey <- c()
  }

  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      mutate(incidence = incidence / populationSize * 100000)
    pCasesTitle <- str_c(pCasesTitle, " / 100'000")
  }

  caseData <- caseData %>%
    filter(data_type == "Confirmed cases") %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, !!!focusRegionRecodeKey))

  if (!is.null(focusRegion)) {
    caseData <- caseData %>%
      mutate(region = as_factor(region)) %>%
      mutate(region = fct_relevel(region, translator$t(focusRegionLong), after = Inf))
  }

  if (caseAverage > 1) {
    caseData <- caseData %>%
      group_by(data_type, country, region) %>%
      mutate(
        incidence = slide_index_dbl(incidence, date, mean, .before = lubridate::days(caseAverage))
      ) %>%
      ungroup()
    pCasesTitle <- str_c(pCasesTitle, "\n(", translator$t("7 day avarage"), ")")
  }

  estimatesPlot <- estimates %>%
    filter(data_type == "Confirmed cases") %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, !!!focusRegionRecodeKey))

  if (!is.null(focusRegion)) {
    estimatesPlot <- estimatesPlot %>%
      mutate(region = as_factor(region)) %>%
      mutate(region = fct_relevel(region, translator$t(focusRegionLong), after = Inf))
  }

  if (logCaseYaxis) {
    zoomRange <- makeZoomRange(log10(max(caseData$incidence, na.rm = TRUE)), extra = log10(5), stepSize = log10(10))
  } else {
    zoomRange <- makeZoomRange(max(caseData$incidence, na.rm = TRUE))
  }

  if (caseDataRightTruncation > 0) {
    caseDataTrunc <- caseData %>%
      group_by(region) %>%
      filter(date <= max(date) - caseDataRightTruncation)
    caseDataRest <- caseData %>%
      group_by(region) %>%
      filter(date > max(date) - caseDataRightTruncation) %>%
      mutate(region_plot = str_c(region, " truncated"))
  } else {
    caseDataTrunc <- caseData
  }

  pCases <- plot_ly(data = caseDataTrunc) %>%
    add_bars(x = ~date, y = ~incidence, color = ~region, colors = regionColors,
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
      legend = list(title = list(text = str_c("<b>", translator$t(regionTitle), "</b>"))),
      sliders = list(
        makeSlider(zoomRange)
      )
    )

  if (caseDataRightTruncation > 0) {
    pCases <- pCases %>%
      add_bars(
        data = caseDataRest,
        x = ~date, y = ~incidence, color = ~region_plot,
        colors = regionColors,
        text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
          round(incidence, 3), " ", toLowerFirst(data_type),
          if_else(caseNormalize, " / 100'000", ""),
          if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
          "<br>(not used for R<sub>e</sub> estimates)<extra></extra>"),
        hovertemplate = "%{text}",
        legendgroup = ~region, showlegend = FALSE)
  }

  if (caseLoess) {
    caseDataTruncLoess <- caseDataTrunc %>%
      filter(data_type != "Excess deaths") %>%
      group_by(country, region, source, data_type) %>%
      mutate(incidenceLoess = getLOESSCases(date, incidence))
    pCases <- pCases %>%
      add_trace(data = caseDataTruncLoess,
        x = ~date, y = ~incidenceLoess, color = ~region, color = regionColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~region, showlegend = FALSE,
        hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      add_trace(
        data = filter(caseData, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~region, color = regionColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> deconvoluted Data +/- sd.</i><extra></extra>"),
        legendgroup = ~region, showlegend = FALSE,
        hovertemplate = "%{text}") %>%
      add_ribbons(
        data =  filter(caseData, !is.na(deconvoluted)),
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~region, colors = regionColors,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~region, showlegend = FALSE,
        hoverinfo = "none")
  }

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~region, colors = regionColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~region,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", region, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
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
          text = str_c("<b>", translator$t(regionTitle), "</b>"))
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
    config(doubleClick = "reset", displaylogo = FALSE, modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1, filename = "ReEstimates"),
      locale = locale, scrollZoom = FALSE)

  if(!is.null(focusRegion)) {
    plot <- plotlyShowTraces(plot, focusRegionLong)
  }

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
  caseDataRightTruncation = 2,
  logCaseYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
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
    xrNote <- 0.9
    yrNote <- 0.4
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
    bottomMargin <- 80
    rightMargin <- 200

  # prepare Data
  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      mutate(incidence = incidence / populationSize * 100000)
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

  estimatesPlot <- estimates

  if (logCaseYaxis) {
    zoomRange <- makeZoomRange(log10(max(caseData$incidence, na.rm = TRUE)), extra = log10(5), stepSize = log10(10))
  } else {
    zoomRange <- makeZoomRange(max(caseData$incidence, na.rm = TRUE))
  }

  if (caseDataRightTruncation > 0) {
    caseDataTrunc <- caseData %>%
      group_by(country) %>%
      filter(date <= max(date) - caseDataRightTruncation)
    caseDataRest <- caseData %>%
      group_by(country) %>%
      filter(date > max(date) - caseDataRightTruncation) %>%
      mutate(country_plot = str_c(country, " truncated"))
  } else {
    caseDataTrunc <- caseData
  }

  pCases <- plot_ly(data = caseDataTrunc) %>%
    add_bars(x = ~date, y = ~incidence, color = ~country, colors = countryColors,
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

  if (caseDataRightTruncation > 0) {
    pCases <- pCases %>%
      add_bars(
        data = caseDataRest,
        x = ~date, y = ~incidence, color = ~country_plot,
        colors = countryColors,
        text = ~str_c("<i>", format(date, dateFormatLong), "</i> <br>",
          round(incidence, 3), " ", toLowerFirst(data_type),
          if_else(caseNormalize, " / 100'000", ""),
          if_else(caseAverage > 1, str_c(" (", caseAverage, " day average)"), ""),
          "<br>(not used for R<sub>e</sub> estimates)<extra></extra>"),
        hovertemplate = "%{text}",
        legendgroup = ~country, showlegend = FALSE)
  }

  if (caseLoess) {
    caseDataTruncLoess <- caseDataTrunc %>%
      filter(data_type != "Excess deaths") %>%
      group_by(country, region, source, data_type) %>%
      mutate(incidenceLoess = getLOESSCases(date, incidence))
    pCases <- pCases %>%
      add_trace(data = caseDataTruncLoess,
        x = ~date, y = ~incidenceLoess, color = ~country, color = countryColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~country, showlegend = FALSE,
        hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      add_trace(
        data = filter(caseData, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~country, color = countryColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> deconvoluted Data +/- sd.</i><extra></extra>"),
        legendgroup = ~country, showlegend = FALSE,
        hovertemplate = "%{text}") %>%
      add_ribbons(
        data =  filter(caseData, !is.na(deconvoluted)),
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~country, colors = countryColors,
        line = list(color = "transparent"), opacity = 0.5,
        legendgroup = ~country, showlegend = FALSE,
        hoverinfo = "none")
  }

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~country, colors = countryColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~country,
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", country, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
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
      " <br>(", country, ")"),
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
    config(doubleClick = "reset", displaylogo = FALSE, modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1, filename = "ReEstimates"),
      locale = locale, scrollZoom = FALSE)

  plot$elementId <- widgetID

  if(!is.null(focusCountry)) {
    plot <- plotlyShowTraces(plot, focusCountry)
  }

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

plotlyShowTraces <- function(plot, traceName, mode = "only") {
  for (i in seq_len(length(plot$x$data))) {
    if (mode == "only" & !str_detect(plot$x$data[[i]]$name, fixed(traceName)) & plot$x$data[[i]]$yaxis != "y3") {
       plot$x$data[[i]]$visible <- "legendonly"
    } else if (mode == "not" & str_detect(plot$x$data[[i]]$name, fixed(traceName)) & plot$x$data[[i]]$yaxis != "y3") {
      plot$x$data[[i]]$visible <- "legendonly"
    }
  }
  return(plot)
}

dataUpdatesString <- function(latestData, name = "Data Source", dateFormat = "%Y-%m-%d") {
  latestDataSum <- latestData %>%
    group_by(source, lastChanged) %>%
    summarize(
      data_type = str_c(data_type, collapse = ", "),
      .groups = "keep")
  
  outList <- list(str_c(name, ": "))
  nSources <- dim(latestDataSum)[1]
  if (nSources == 1) {
    outList[[2]] <- str_c(
      latestDataSum$source, ", ", format(latestDataSum$lastChanged, dateFormat),
      "; ")
  } else {
    for (i in seq_len(nSources)) {
      outList[[i + 1]] <- str_c(
        latestDataSum[i, ]$source, ", ", format(latestDataSum[i, ]$lastChanged, dateFormat),
        " (", latestDataSum[i, ]$data_type, ")",
        "; ")
    }
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}

toLowerFirst <- function(string) {
  str_replace(string, ".{1}", tolower(str_extract(string, ".{1}")))
}
