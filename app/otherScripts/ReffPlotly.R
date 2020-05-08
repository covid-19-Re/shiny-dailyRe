# TO-DO: deduplicate code & general clean up...

toLowerFirst <- function(string) {
  str_replace(string, ".{1}", tolower(str_extract(string, ".{1}")))
}

rEffPlotly <- function(
  caseData,
  estimates,
  interventions,
  plotColoursNamed,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date),
  legendOrientation = "v", # "v" or "h"
  textElements,
  language,
  widgetID = "rEffplots") {

  # plot parameter
  if (language %in% c("de-ch", "fr-ch")) {
    locale <- language
    dateFormat <- "%d. %b"
    dateFormatLong <- "%d.%m.%Y"
  } else if (language == "en-gb") {
    locale <- NULL
    dateFormat <- "%b-%d"
    dateFormatLong <- "%Y-%m-%d"
  } else if (language == "it-ch") {
    locale <- "it"
    dateFormat <- "%d. %b"
    dateFormatLong <- "%d.%m.%Y"
  }

  names(plotColoursNamed) <- c(
    textElements[[language]][["confirmedCases"]],
    textElements[[language]][["hospPatients"]],
    textElements[[language]][["deaths"]])

  lastDataDate$source[1] <- textElements[[language]][["FOPH"]]

  axisTitleFontSize <- 14
  if (legendOrientation == "v") {
    xrNote <- 1
    yrNote <- 0.35
    rNote <- textElements[[language]][["rEexplanation"]]
    rNoteAnchors <- c("left", "bottom")
    xHelpBox <- 1
    yHelpBox <- 0.8
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 174
    hHelpBox <- 90
    if (language %in% c("fr-ch", "it-ch")) {
      hHelpBox <- 120
    } else if (language == "de-ch") {
      hHelpBox <- 130
    }
    helpBoxText <- textElements[[language]][["helpBox"]]
    helpBoxShift <- c(10, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80
  } else if (legendOrientation == "h") {
    xrNote <- 0.99
    yrNote <- 0.34
    rNote <- textElements[[language]][["rEexplanation"]]
    rNoteAnchors <- c("right", "bottom")
    xHelpBox <- 0
    yHelpBox <- -0.15
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 550
    hHelpBox <- 30
    helpBoxText <- textElements[[language]][["helpBox"]]
    helpBoxShift <- c(0, 0)
    xDataSource <- 0
    yDataSource <- -0.21
    dataSourceAnchors <- c("left", "top")
    bottomMargin <- 140
  } else {
    stop("legendOrientation must be either \"v\" or \"h\".")
  }

  # prepare Data
  newLevelNames <- c(textElements[[language]][["confirmedCases"]],
    textElements[[language]][["hospPatients"]],
    textElements[[language]][["deaths"]])
  newLevels <- c("Confirmed cases",  "Hospitalized patients", "Deaths")
  names(newLevels) <- newLevelNames

  caseData <- caseData %>%
    filter(region == "Switzerland") %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels))

  estimatesPlot <- estimates %>%
    filter(
      region == "Switzerland") %>%
    mutate(data_type = fct_recode(data_type, !!!newLevels))

  estimatesEndPoint <- estimatesPlot %>%
    group_by(data_type) %>%
    filter(date == max(date))

  pCases <- plot_ly(data = caseData) %>%
    add_bars(x = ~date, y = ~incidence, color = ~data_type,
      colors = plotColoursNamed,
      text = ~str_c("<i>", format(date, "%d.%m.%y"), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}",
      legendgroup = ~data_type) %>%
    layout(
      xaxis = list(title = "",
        type = "date",
        range = c(startDate, endDate),
        tickvals = seq(startDate, endDate, length.out = 18),
        dtick = 3 * 86400000,
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(
        fixedrange = TRUE,
        title = list(text = textElements[[language]][["axisCases"]], font = list(size = axisTitleFontSize))),
      legend = list(title = list(text = "<b> Data Type </b>")))

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColoursNamed,
      type = "scatter", mode = "lines",
      legendgroup = ~data_type, showlegend = FALSE,
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
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
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~data_type, colors = plotColoursNamed,
      legendgroup = ~data_type,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", data_type, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(textElements[[language]][["rLarger1"]], textElements[[language]][["rLower1"]]),
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
        #tick0 = startDate,
        tickvals = seq(startDate, endDate, length.out = 18),
        #dtick = 3 * 86400000,
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(
        range = c(0, 2),
        fixedrange = TRUE,
        title = list(
          text = textElements[[language]][["axisRe"]],
          font = list(size = axisTitleFontSize)),
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", textElements[[language]][["dataType"]], "</b>"))
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
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(visible = FALSE, fixedrange = TRUE))

  plotlist <- list(pCases, pEstimates, pIntervention)
  plot <- subplot(plotlist, nrows = 3, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
    layout(
      margin = list(b = bottomMargin),
      legend = list(orientation = legendOrientation),
      annotations = list(
        list(
          x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate, name = textElements[[language]][["dataSource"]], dateFormatLong),
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
      locale = locale)

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
  minEstimateDate = as.Date("2020-03-07"),
  legendOrientation = "v", # "v" or "h"
  regionColors,
  textElements,
  language,
  widgetID = "rEffplotsRegions") {

  # plot parameter
  if (language %in% c("de-ch", "fr-ch")) {
    locale <- language
    dateFormat <- "%d. %b"
    dateFormatLong <- "%d.%m.%Y"
  } else if (language == "en-gb") {
    locale <- NULL
    dateFormat <- "%b-%d"
    dateFormatLong <- "%Y-%m-%d"
  } else if (language == "it-ch") {
    locale <- "it"
    dateFormat <- "%d. %b"
    dateFormatLong <- "%d.%m.%Y"
  }

  lastDataDate$source[1] <- textElements[[language]][["FOPH"]]

  axisTitleFontSize <- 14
  if (legendOrientation == "v") {
    xrNote <- 0.99
    yrNote <- 0.60
    rNote <- textElements[[language]][["rEexplanation"]]
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
    helpBoxText <- textElements[[language]][["helpBox"]]
    helpBoxShift <- c(10, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80
    rightMargin <- 200
  } else if (legendOrientation == "h") {
    xrNote <- 0.99
    yrNote <- 0.60
    rNote <- textElements[[language]][["rEexplanation"]]
    rNoteAnchors <- c("right", "top")
    xHelpBox <- 0
    yHelpBox <- -0.2
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 400
    hHelpBox <- 50
    helpBoxText <- textElements[[language]][["helpBoxH"]]
    helpBoxShift <- c(0, 0)
    xDataSource <- 0
    yDataSource <- -0.21
    dataSourceAnchors <- c("left", "top")
    bottomMargin <- 200
  } else {
    stop("legendOrientation must be either \"v\" or \"h\".")
  }

  # prepare Data
  newLevelNames <- c(textElements[[language]][["confirmedCases"]],
    textElements[[language]][["hospPatients"]],
    textElements[[language]][["deaths"]])
  newLevels <- c("Confirmed cases",  "Hospitalized patients", "Deaths")
  names(newLevels) <- newLevelNames

  names(regionColors) <- recode(names(regionColors), Switzerland = textElements[[language]][["totalCH"]])

  caseData <- caseData %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, Switzerland = textElements[[language]][["totalCH"]])
      ) %>%
    filter(data_type == textElements[[language]][["confirmedCases"]])

  caseDataCH <- filter(caseData, region == textElements[[language]][["totalCH"]])

  estimatesPlot <- estimates %>%
    filter(
      date >= minEstimateDate) %>%
    mutate(
      data_type = fct_recode(data_type, !!!newLevels),
      region = recode(region, Switzerland = textElements[[language]][["totalCH"]])
      ) %>%
    filter(data_type == textElements[[language]][["confirmedCases"]])

  estimatesPlotCH <- filter(estimatesPlot, region == textElements[[language]][["totalCH"]])

  estimatesEndPoint <- estimatesPlot %>%
    group_by(data_type) %>%
    filter(date == max(date))

  pCases <- plot_ly(data = caseData) %>%
    filter(region != textElements[[language]][["totalCH"]]) %>%
    add_bars(x = ~date, y = ~incidence, color = ~region, colors = regionColors,
      legendgroup = ~region, visible = "legendonly",
      text = ~str_c("<i>", format(date, "%d.%m.%y"), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_bars(data = caseDataCH, x = ~date, y = ~incidence, color = ~region, colors = regionColors,
      legendgroup = ~region,
      text = ~str_c("<i>", format(date, "%d.%m.%y"), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}") %>%
    layout(
      xaxis = list(title = "",
        type = "date",
        range = c(startDate, endDate),
        tick0 = startDate,
        dtick = 3 * 86400000,
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(
        fixedrange = TRUE,
        title = list(text = textElements[[language]][["axisCases"]], font = list(size = axisTitleFontSize))),
      legend = list(title = list(text = "<b> Data Type </b>")))

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    filter(region != textElements[[language]][["totalCH"]]) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~region, colors = regionColors,
      type = "scatter", mode = "lines", showlegend = FALSE,
      legendgroup = ~region, visible = "legendonly",
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")<extra></extra>"),
      hovertemplate = "%{text}") %>%
    add_ribbons(
      x = ~date, ymin = ~median_R_lowHPD, ymax = ~median_R_highHPD,
      color = ~region, legendgroup = ~region, visible = "legendonly",
      line = list(color = "transparent"), opacity = 0.5, showlegend = FALSE,
      hoverinfo = "none") %>%
    group_by(region) %>%
    filter(date == max(date)) %>%
    add_trace(
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers", visible = "legendonly",
      color = ~region, colors = regionColors,
      legendgroup = ~region,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
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
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
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
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", region, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(textElements[[language]][["rLarger1"]], textElements[[language]][["rLower1"]]),
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
      xaxis = list(title = "",
        type = "date",
        range = c(startDate, endDate),
        #tick0 = startDate,
        tickvals = seq(startDate, endDate, length.out = 18),
        #dtick = 3 * 86400000,
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(
        range = c(0, 3),
        fixedrange = TRUE,
        title = list(
          text = textElements[[language]][["axisRe"]],
          font = list(size = axisTitleFontSize)),
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", textElements[[language]][["canton"]], "</b>"))
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
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(visible = FALSE, fixedrange = TRUE))

  plotlist <- list(pCases, pEstimates, pIntervention)
  plot <- subplot(plotlist, nrows = 3, shareX = TRUE, titleY = TRUE, margin = c(0, 0, 0.02, 0)) %>%
    layout(
      margin = list(b = bottomMargin, r = rightMargin),
      legend = list(orientation = legendOrientation),
      annotations = list(
        list(
          x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate, name = textElements[[language]][["dataSource"]], dateFormatLong),
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
      locale = locale)

  plot$elementId <- widgetID

  return(plot)
}

rEffPlotlyCountry <- function(
  countrySelect,
  caseData,
  estimates,
  plotColoursNamed,
  lastDataDate,
  startDate = min(caseData$date) - 1,
  endDate = max(caseData$date),
  legendOrientation = "v", # "v" or "h"
  textElements,
  language,
  widgetID = "rEffplotsCountry") {

  # plot parameter
  if (language %in% c("de-ch", "fr-ch")) {
    locale <- language
    dateFormat <- "%d. %b"
    dateFormatLong <- "%d.%m.%Y"
  } else if (language == "en-gb") {
    locale <- NULL
    dateFormat <- "%b-%d"
    dateFormatLong <- "%Y-%m-%d"
  } else if (language == "it-ch") {
    locale <- "it"
    dateFormat <- "%d. %b"
    dateFormatLong <- "%d.%m.%Y"
  }

  names(plotColoursNamed) <- c(
    textElements[[language]][["confirmedCases"]],
    textElements[[language]][["hospPatients"]],
    textElements[[language]][["deaths"]],
    textElements[[language]][["excessDeaths"]])

  lastDataDate <- filter(lastDataDate, country == countrySelect)

  axisTitleFontSize <- 14
  if (legendOrientation == "v") {
    xrNote <- 1
    yrNote <- 0.35
    rNote <- textElements[[language]][["rEexplanation"]]
    rNoteAnchors <- c("left", "bottom")
    xHelpBox <- 1
    yHelpBox <- 0.8
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 174
    hHelpBox <- 90
    if (language %in% c("fr-ch", "it-ch")) {
      hHelpBox <- 120
    } else if (language == "de-ch") {
      hHelpBox <- 130
    }
    helpBoxText <- textElements[[language]][["helpBox"]]
    helpBoxShift <- c(10, 0)
    xDataSource <- 1
    yDataSource <- -0.1
    dataSourceAnchors <- c("right", "auto")
    bottomMargin <- 80
  } else if (legendOrientation == "h") {
    xrNote <- 0.99
    yrNote <- 0.34
    rNote <- textElements[[language]][["rEexplanation"]]
    rNoteAnchors <- c("right", "bottom")
    xHelpBox <- 0
    yHelpBox <- -0.15
    helpBoxAnchors <- c("left", "top")
    wHelpBox <- 550
    hHelpBox <- 30
    helpBoxText <- textElements[[language]][["helpBox"]]
    helpBoxShift <- c(0, 0)
    xDataSource <- 0
    yDataSource <- -0.21
    dataSourceAnchors <- c("left", "top")
    bottomMargin <- 140
  } else {
    stop("legendOrientation must be either \"v\" or \"h\".")
  }

  caseDataPlot <- caseData %>%
    filter(country == countrySelect, region == countrySelect)

  estimatesPlot <- estimates %>%
    filter(country == countrySelect, region == countrySelect)

  estimatesEndPoint <- estimatesPlot %>%
    group_by(data_type) %>%
    filter(date == max(date))

  pCases <- plot_ly(data = caseData) %>%
    add_bars(x = ~date, y = ~incidence, color = ~data_type,
      colors = plotColoursNamed,
      text = ~str_c("<i>", format(date, "%d.%m.%y"), "</i> <br>",
        incidence, " ", toLowerFirst(data_type), "<extra></extra>"),
      hovertemplate = "%{text}",
      legendgroup = ~data_type) %>%
    layout(
      xaxis = list(title = "",
        type = "date",
        range = c(startDate, endDate),
        tickvals = seq(startDate, endDate, length.out = 18),
        dtick = 3 * 86400000,
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(
        fixedrange = TRUE,
        title = list(text = textElements[[language]][["axisCases"]], font = list(size = axisTitleFontSize))),
      legend = list(title = list(text = "<b> Data Type </b>")))

  pEstimates <- plot_ly(data = estimatesPlot) %>%
    add_trace(
      x = ~date, y = ~median_R_mean, color = ~data_type, colors = plotColoursNamed,
      type = "scatter", mode = "lines",
      legendgroup = ~data_type, showlegend = FALSE,
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
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
      x = ~as.POSIXct(date) + 10 * 60 * 60, y = ~median_R_mean,
      type = "scatter", mode = "markers",
      color = ~data_type, colors = plotColoursNamed,
      legendgroup = ~data_type,
      marker = list(symbol = "asterisk-open"),
      text = ~str_c("<i>", format(date, "%d.%m.%y"),
      "</i> <br> R<sub>e</sub>: ", signif(median_R_mean, 3),
      " (", signif(median_R_lowHPD, 3), "-", signif(median_R_highHPD, 3), ")",
      " <br>(", data_type, ")"),
      hoverinfo = "text",
      showlegend = FALSE) %>%
    add_annotations(
      text = c(textElements[[language]][["rLarger1"]], textElements[[language]][["rLower1"]]),
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
        #tick0 = startDate,
        tickvals = seq(startDate, endDate, length.out = 18),
        #dtick = 3 * 86400000,
        tickformat = dateFormat,
        tickangle = 45,
        showgrid = TRUE,
        fixedrange = TRUE),
      yaxis = list(
        range = c(0, 2),
        fixedrange = TRUE,
        title = list(
          text = textElements[[language]][["axisRe"]],
          font = list(size = axisTitleFontSize)),
        zeroline = TRUE),
      legend = list(
        title = list(
          text = str_c("<b>", textElements[[language]][["dataType"]], "</b>"))
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
      margin = list(b = bottomMargin),
      legend = list(orientation = legendOrientation),
      annotations = list(
        list(
          x = xDataSource, y = yDataSource, xref = "paper", yref = "paper",
          text = dataUpdatesString(lastDataDate, name = textElements[[language]][["dataSource"]], dateFormatLong),
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
      locale = locale)

  plot$elementId <- widgetID

  return(plot)
}
