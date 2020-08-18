# TO-DO: deduplicate code & general clean up...
library(viridisLite)
library(shades)

# plot colors
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
casesSubPlot <- function(
  caseData,
  seriesColors,
  seriesTitle = "Data types",
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date) + 1,
  caseDataRightTruncation = 2,
  fixedRangeX = TRUE,
  fixedRangeY =TRUE,
  logYaxis = FALSE,
  caseAverage = 1,
  caseNormalize = FALSE,
  caseLoess = FALSE,
  caseDeconvoluted = FALSE,
  translator,
  dateFormat = "%b-%d",
  dateFormatLong = "%Y-%m-%d") {

  if (!("testPositivity" %in% colnames(caseData))) {
    caseData$testPositivity <- NA
    caseData$totalTests <- NA
    caseData$positiveTests <- NA
    caseData$negativeTests <- NA
  }

  pCasesTitle <- translator$t("New observations")

  if (caseNormalize) {
    caseData <- caseData %>%
      mutate(value = value / populationSize * 100000)
    pCasesTitle <- str_c(pCasesTitle, " / 100'000")
  }

  if (caseAverage > 1) {
    caseData <- caseData %>%
      group_by(data_type) %>%
      mutate(
        value = slide_index_dbl(value, date, mean, .before = lubridate::days(caseAverage))
      ) %>%
      ungroup()
    pCasesTitle <- str_c(pCasesTitle, "\n(", translator$t("7 day avarage"), ")")
  }

  if (logYaxis) {
    zoomRange <- makeZoomRange(log10(max(caseData$value, na.rm = TRUE)), extra = log10(5), stepSize = log10(10))
  } else {
    zoomRange <- makeZoomRange(max(caseData$value, na.rm = TRUE))
  }

  caseData <- caseData %>%
    group_by(series) %>%
    mutate(
      tooltipText = str_c("<i>", format(date, dateFormatLong), "</i> <br>",
        round(value, 3), " ", toLowerFirst(data_type),
        if_else(caseNormalize, " / 100'000", ""),
        if_else(caseAverage > 1, str_c(" (", caseAverage, " ", "day average", ")"), ""),
        if_else(data_type == "Confirmed cases" & !is.na(testPositivity),
          str_c("<br>Test positivity ", round(testPositivity, 3), " (", positiveTests, " / ", negativeTests, ")"),
          ""
        ),
        if_else(data_type == "Confirmed cases / tests",
          str_c("<br>", value * totalTests, " cases",
            "<br>Test positivity ", round(testPositivity, 3), " (", positiveTests, " / ", negativeTests, ")"
          ),
          ""
        ))
    )

  if (caseDataRightTruncation > 0) {
    caseDataTrunc <- caseData %>%
      group_by(series) %>%
      filter(date <= max(date) - caseDataRightTruncation)
    caseDataRest <- caseData %>%
      group_by(series) %>%
      filter(date > max(date) - caseDataRightTruncation) %>%
      mutate(series_plot = str_c(series, " truncated"))
  } else {
    caseDataTrunc <- caseData
  }

  pCases <- plot_ly(data = caseDataTrunc) %>%
    add_bars(x = ~date, y = ~value, color = ~series,
      colors = seriesColors,
      text = ~str_c(tooltipText, "<extra></extra>"),
      hovertemplate = "%{text}",
      legendgroup = ~series) %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX, rSlider = FALSE, rSelector = FALSE),
      yaxis = plotlyYaxis(
        title = pCasesTitle,
        fixedRange = fixedRangeY,
        logAxis = logYaxis),
      legend = list(title = list(text = str_c("<b>", translator$t(seriesTitle), "</b>"))),
      sliders = list(
        makeSlider(zoomRange)
      )
    )

  if (caseDataRightTruncation > 0) {
    pCases <- pCases %>%
      add_bars(
        data = caseDataRest,
        x = ~date, y = ~value, color = ~series_plot,
        colors = seriesColors,
        text = ~str_c(tooltipText, "<br>(not used for R<sub>e</sub> estimates)<extra></extra>"),
        hovertemplate = "%{text}", inherit = FALSE,
        legendgroup = ~series, showlegend = FALSE)
  }

  if (caseLoess) {
    caseDataTruncLoess <- caseDataTrunc %>%
      filter(data_type != "Excess deaths") %>%
      group_by(country, region, source, data_type) %>%
      mutate(incidenceLoess = getLOESSCases(date, value))
    pCases <- pCases %>%
      add_trace(
        data = caseDataTruncLoess,
        x = ~date, y = ~incidenceLoess, color = ~series, color = seriesColors,
        type = "scatter", mode = "lines", opacity = 0.5,
        text = ~str_c("<i> Loess Fit </i><extra></extra>"),
        legendgroup = ~series, showlegend = FALSE,
        hovertemplate = "%{text}")
  }

  if (caseDeconvoluted) {
    pCases <- pCases %>%
      add_trace(
        data = filter(caseData, !is.na(deconvoluted)),
        x = ~date, y = ~deconvoluted, color = ~series, color = seriesColors,
        type = "scatter", mode = "lines",
        text = ~str_c("<i> deconvoluted data +/- sd.</i><extra></extra>"),
        legendgroup = ~series, showlegend = FALSE,
        hovertemplate = "%{text}") %>%
      add_ribbons(
        x = ~date, ymin = ~deconvolutedLow, ymax = ~deconvolutedHigh,
        color = ~series, colors = seriesColors,
        opacity = 0.2,
        legendgroup = ~series, showlegend = FALSE,
        hoverinfo = "none")
  }

  return(pCases)
}

estimatesSubPlot <- function(
  estimates,
  seriesColors,
  seriesTitle,
  startDate,
  endDate,
  fixedRangeX = TRUE,
  fixedRangeY = TRUE,
  translator,
  dateFormat,
  dateFormatLong) {

  pEstimates <- plot_ly(data = estimates) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~series, colors = seriesColors,
      type = "scatter", mode = "lines",
      legendgroup = ~series, showlegend = FALSE,
      text = ~str_c("<i>", format(date, dateFormatLong),
        "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
        " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
        " <br>(", series, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~series, colors = seriesColors,
      line = list(color = "transparent"), opacity = 0.5,
      legendgroup = ~series, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(series) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~series, colors = seriesColors,
      legendgroup = ~series,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, dateFormatLong),
      "</i> <br> R<sub>e</sub>: ", round(median_R_mean, 2),
      " (", round(median_R_lowHPD, 2), "-", round(median_R_highHPD, 2), ")",
      " <br>(", series, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(
        translator$t("Exponential increase<br>in number of new cases"),
        translator$t("Decrease in number of new cases")
        ),
      font = list(color = "red"),
      x = startDate,
      y = c(1.30, 0.85),
      textangle = 0,
      align = "left",
      xanchor = "left",
      yanchor = "middle",
      showarrow = FALSE,
      inherit = FALSE) %>%
    add_annotations(
      text =  translator$t(str_c(
        "<b>*</b>&nbsp;This is the most recent possible R<sub>e</sub> estimate due to ",
        "delays between infection and ",
        "the last data observation.")),
      font = list(size = 10, color = "#505050"),
      x = endDate,
      y = 0,
      textangle = 0,
      align = "left",
      xanchor = "right",
      yanchor = "bottom",
      showarrow = FALSE,
      inherit = FALSE) %>%
    layout(
      xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX),
      yaxis = plotlyYaxis(
        title = translator$t("Reproductive number R<sub>e</sub>"),
        range = c(0, 4),
        fixedRange = fixedRangeY,
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", translator$t(seriesTitle), "</b>"))
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
  return(pEstimates)
}

interventionsSubPlot <- function(
  interventions,
  startDate,
  endDate,
  fixedRangeX = TRUE,
  fixedRangeY = TRUE,
  dateFormat) {
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
        xaxis = plotlyXaxis(startDate, endDate, dateFormat, fixedRangeX),
        yaxis = plotlyYaxis(visible = FALSE, fixedRange = fixedRangeY))
    return(pIntervention)
}

rEffPlotly <- function(
  caseData,
  estimates,
  interventions,
  seriesName = "data_type",
  seriesColors,
  seriesTitle = "Data types",
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
  showHelpBox = TRUE,
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

  newSeriesNames <- str_split(names(seriesColors), pattern = "\\s(?=truncated)", simplify = TRUE)
  newSeriesNames[, 1] <- sapply(newSeriesNames[, 1], translator$t,  USE.NAMES = FALSE)
  names(seriesColors) <- str_trim(str_c(newSeriesNames[, 1], newSeriesNames[, 2], sep = " "))

  # layout pars
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

  renameDataType <- sapply(unique(as.character(caseData$data_type)), translator$t,  USE.NAMES = TRUE)

  caseData <- caseData %>%
    mutate(
      data_type = recode(as.character(data_type), !!!renameDataType))
  estimates <- estimates %>%
    mutate(
      data_type = recode(as.character(data_type), !!!renameDataType))

  if (seriesName %in% colnames(caseData)) {
    caseData$series <- caseData[[seriesName]]
    estimates$series <- estimates[[seriesName]]
  } else {
    stop("Series not in Data!")
  }
  
  # cases plot
  pCases <- casesSubPlot(
    caseData,
    seriesColors,
    seriesTitle,
    startDate,
    endDate,
    caseDataRightTruncation,
    fixedRangeX = fixedRangeX[1],
    fixedRangeY = fixedRangeY[1],
    logYaxis = logCaseYaxis,
    caseAverage,
    caseNormalize,
    caseLoess,
    caseDeconvoluted,
    translator,
    dateFormat,
    dateFormatLong)

  pEstimates <- estimatesSubPlot(
    estimates,
    seriesColors,
    seriesTitle,
    startDate,
    endDate,
    fixedRangeX = fixedRangeX[2],
    fixedRangeY = fixedRangeY[2],
    translator,
    dateFormat,
    dateFormatLong)

  if (!is.null(interventions)) {
    pIntervention <- interventionsSubPlot(
      interventions,
      startDate,
      endDate,
      fixedRangeX[3],
      fixedRangeY[3],
      dateFormat)
    plotlist <- list(pCases, pEstimates, pIntervention)
    nPlots <- 3
  } else {
    plotlist <- list(pCases, pEstimates)
    nPlots <- 2
  }

  plotAnnotations <- list(
    list(
      x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
      text = dataUpdatesString(lastDataDate, name = translator$t("Data Source"), dateFormatLong),
      showarrow = FALSE,
      xanchor = dataSourceAnchors[1], yanchor = dataSourceAnchors[2], xshift = 0, yshift = 0,
      font = list(size = 10, color = "black"))
  )

  if (showHelpBox) {
    plotAnnotations[[3]] <- list(
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
  }

  plot <- subplot(plotlist, nrows = nPlots, shareX = TRUE, titleY = TRUE) %>%
    layout(
      margin = list(r = 200, b = bottomMargin),
      annotations = plotAnnotations
    ) %>%
    config(doubleClick = "reset", displaylogo = FALSE, modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1, filename = "ReEstimates"),
      locale = locale, scrollZoom = FALSE)

  plot$elementId <- widgetID

  if (!is.null(showTraces)) {
    plot <- plotlyShowTraces(plot, showTraces, mode = showTracesMode)
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

renameRegionTotal <- function(data, countries, countryNames) {
  renamedData <- data %>%
      mutate(region = fct_relevel(
        as_factor(
          str_replace(region, pattern = countries, replacement = str_c(countryNames, " (Total)"))
        ),
        str_c(countryNames, " (Total)"),
        after = Inf)
      )
  return(renamedData)
}

rEffPlotlyShiny <- function(countryData, updateData, interventions, seriesSelect, input, translator) {
  countries <- unique(countryData$estimates$countryIso3)
  countryNames <- unique(countryData$estimates$country)
  nCountries <- length(countries)

  if (nCountries == 1) {
    seriesName <- seriesSelect
    interventions <- interventions[[countries]]
    if(!is.null(interventions)){
      interventions <- interventions %>%
        mutate(
          text = sapply(text, translator$t,  USE.NAMES = FALSE),
          tooltip =  sapply(tooltip, translator$t,  USE.NAMES = FALSE)
        )
    }
     
    if (seriesName == "data_type") {
      seriesTitle <- "Data types"
      seriesColors <- plotColors
      dataTypeSelect <- unique(countryData$estimates$data_type)
      regionSelect <- countries
    } else if (seriesName == "region"){
      seriesTitle <- case_when(
        countries == "CHE" ~ translator$t("Canton"),
        countries == "ZAF" ~ translator$t("Province"),
        TRUE ~ "Region")

      validate(need(!is.null(input$dataTypeSelect), message = "loading..."))
      dataTypeSelect <- input$dataTypeSelect

      regionSelect <- str_subset(unique(countryData$estimates$region), pattern = "grR", negate = TRUE)
      seriesColors1 <- viridis(length(regionSelect))
      names(seriesColors1) <- regionSelect
      seriesColorsTrunc <- saturation(seriesColors1, value = 0.1)
      names(seriesColorsTrunc) <- str_c(names(seriesColors1), " truncated")
      seriesColors <- c(seriesColors1, seriesColorsTrunc)

    } else if (seriesName == "greaterRegion") {
      seriesName <- "region"
      seriesTitle <- "Greater Region"

      validate(need(!is.null(input$dataTypeSelect), message = "loading..."))
      dataTypeSelect <- input$dataTypeSelect

      regionSelect <- c(str_subset(unique(countryData$estimates$region), pattern = "grR", negate = FALSE), countries)
      seriesColors1 <- viridis(length(regionSelect))
      names(seriesColors1) <- regionSelect
      seriesColorsTrunc <- saturation(seriesColors1, value = 0.1)
      names(seriesColorsTrunc) <- str_c(names(seriesColors1), " truncated")
      seriesColors <- c(seriesColors1, seriesColorsTrunc)
    }
  } else {
    seriesName <- "country"
    seriesTitle <- "Country"

    interventions <- NULL

    validate(need(!is.null(input$dataTypeSelect), message = "loading..."))
    dataTypeSelect <- input$dataTypeSelect
    regionSelect <- countries

    seriesColors1 <- viridis(nCountries)
    names(seriesColors1) <- unique(countryData$estimates$country)
    seriesColorsTrunc <- saturation(seriesColors1, value = 0.1)
    names(seriesColorsTrunc) <- str_c(names(seriesColors1), " truncated")
    seriesColors <- c(seriesColors1, seriesColorsTrunc)
  }

  caseData <- bind_rows(countryData$caseData) %>%
    filter(
      data_type %in% dataTypeSelect,
      region %in% regionSelect)

  estimates <- bind_rows(countryData$estimates) %>%
    filter(
      data_type %in% dataTypeSelect,
      estimate_type == input$estimationTypeSelect,
      region %in% regionSelect)

  if (seriesSelect == "region") {
    caseData <- renameRegionTotal(caseData, countries, countryNames)
    estimates <- renameRegionTotal(estimates, countries, countryNames)
    names(seriesColors) <- str_replace(names(seriesColors),
      pattern = countries, replacement = str_c(countryNames, " (Total)"))
    seriesColors[str_c(countryNames, " (Total)")] <- "#666666"
  } else if (seriesSelect == "greaterRegion") {
    caseData <- caseData %>%
      mutate(region = str_replace(region, pattern = "grR ", replacement = ""))
    estimates <- estimates %>%
      mutate(region = str_replace(region, pattern = "grR ", replacement = ""))
    names(seriesColors) <- str_replace(names(seriesColors), pattern = "grR ", replacement = "")

    caseData <- renameRegionTotal(caseData, countries, countryNames)
    estimates <- renameRegionTotal(estimates, countries, countryNames)

    names(seriesColors) <- str_replace(names(seriesColors),
      pattern = countries, replacement = str_c(countryNames, " (Total)"))
    seriesColors[str_c(countryNames, " (Total)")] <- "#666666"
  }

  updateDataPlot <- updateData %>%
    filter(
      data_type %in% dataTypeSelect,
      region %in% regionSelect) %>%
    ungroup() %>%
    dplyr::select(-region) %>%
    group_by(countryIso3, country, source, data_type) %>%
    dplyr::summarize(
      lastChanged = max(lastChanged),
      .groups = "keep") %>%
    ungroup()

  plot <- rEffPlotly(
    caseData = caseData,
    estimates = estimates,
    interventions = interventions,
    seriesName = seriesName,
    seriesColors = seriesColors,
    seriesTitle = seriesTitle,
    lastDataDate = updateDataPlot,
    startDate = min(estimates$date) - 14,
    fixedRangeX = fixedRangeX,
    fixedRangeY = fixedRangeY,
    logCaseYaxis = "logCases" %in% input$plotOptions,
    caseAverage = "caseAverage" %in% input$plotOptions,
    caseNormalize = "caseNormalize" %in% input$plotOptions,
    caseLoess = "caseLoess" %in% input$plotOptions,
    caseDeconvoluted = "caseDeconvoluted" %in% input$plotOptions,
    showTraces = "Confirmed cases / tests",
    showTracesMode = "not",
    showHelpBox = FALSE,
    translator = translator,
    language = "en-gb",#input$lang,
    widgetID = NULL)

  return(plot)
}
