# helpers

loadCountryData <- function(iso3, dataDir = "data/countryData") {

  allPaths <- list.files(path = pathToCountryData, recursive = TRUE)

  dataPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-Data.rds"))
  if (!is_empty(dataPath)){
    caseData <- readRDS(file.path(dataDir, dataPath))
    if (nrow(caseData %>% filter(date_type == "report_plotting")) > 0) {
      caseData <- caseData %>%
        filter(date_type == "report_plotting", is.na(local_infection)) 
    } else {
      caseData <- caseData %>%
        dplyr::group_by(date, region, country, countryIso3, source, data_type, populationSize) %>%
        # there should only be one "date_type" but the summing is left in there in case.
        dplyr::summarise(value = sum(value), .groups = "drop") %>% 
        mutate(local_infection = NA)
    }
  } else {
    caseData <- NULL
  }

  deconvolutedDataPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-DeconvolutedData.rds"))
  if (!is_empty(deconvolutedDataPath)) {
    
    deconvolutedData <- readRDS(file.path(dataDir, deconvolutedDataPath)) %>%
      mutate(data_type = str_sub(data_type, 11)) %>%
      group_by(date, region, country, source, data_type, replicate) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      mutate(local_infection = NA) %>% 
      group_by(date, region, country, source, data_type, local_infection) %>%
      summarise(
        deconvoluted = mean(value),
        deconvolutedLow = deconvoluted - sd(value),
        deconvolutedHigh = deconvoluted + sd(value),
        .groups = "keep"
      )
    caseData <- caseData %>%
      left_join(deconvolutedData, by = c("country", "region", "source", "data_type", "local_infection", "date")) %>%
      arrange(countryIso3, region, source, data_type, date)
  }

  estimatesPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-Estimates.rds"))
  if (!is_empty(estimatesPath)) {
    estimates <- readRDS(file.path(dataDir, estimatesPath))
  } else {
    estimates <- NULL
  }

  if (!is.null(caseData)) {
    estimateRanges <- estimateRanges(
      caseData,
      minConfirmedCases = 100,
      delays = delaysDf)
  } else (
    estimateRanges <- NULL
  )


  countryData <- list(
    caseData = caseData,
    estimates = estimates,
    estimateRanges = estimateRanges)

  return(countryData)
}


dataUpdatesTable <- function(
  updateData,
  dateFormat = "%Y-%m-%d",
  showDataType = FALSE) {

  updateData <- bind_rows(updateData) %>%
    group_by(country, source) %>%
    slice(1L)
  showCountry <- length(unique(updateData$country)) > 1

  outList <- list("<table style=\"width:100%\">")
  for (i in 1:dim(updateData)[1]) {
    if (i == 1) {
      printCountry <- showCountry
    } else {
      printCountry <- (showCountry & updateData[i - 1, ]$country != updateData[i, ]$country)
    }

    if (printCountry) {
      countryString <- str_c("<tr><td colspan=\"3\">", updateData[i, ]$country, "</td></tr>")
    } else {
      countryString <- ""
    }

    sourceString <- str_c("<td style = \"font-weight: normal;\">", updateData[i, ]$source, "</td>")
    if (showCountry) {
      sourceString <- str_c("<td>&nbsp;&nbsp;</td>", sourceString)
    }

    outList[[i + 1]] <- str_c(
      countryString,
      "<tr>",
        sourceString,
        "<td style = \"font-weight: normal;font-style: italic;\">",
          format(updateData[i, ]$lastData, dateFormat),
        "</td>",
      "</tr>")
  }
  outList[[i + 2]] <- "</table>"
  out <- str_c(outList, collapse = "")
  out <- str_c(out, "<small style=\"font-weight: normal\"> last check: ", max(updateData$lastChecked), "</small>")
  return(out)
}

#TODO remove this entire right-side boundary on dates on the Re estimates, it's not needed
delaysDf <- tibble(
    data_type = factor(
      c("Confirmed cases", "Confirmed cases / tests", "Hospitalized patients", "Deaths", "Excess deaths"),
      levels = c("Confirmed cases", "Confirmed cases / tests", "Hospitalized patients", "Deaths", "Excess deaths")),
    # delay = c(8, 8, 10, 18, 30))
    delay = c(0, 0, 0, 0, 0))

estimateRanges <- function(
  caseData,
  minConfirmedCases = 100,
  delays = delaysDf) {

  estimateStartDates <- caseData %>%
    group_by(countryIso3, region, source, data_type) %>%
    arrange(countryIso3, region, source, data_type, date) %>%
    filter(
      data_type == "Confirmed cases",
      cumsum(value) > 100) %>%
    filter(date == min(date)) %>%
    ungroup() %>%
    dplyr::select(countryIso3, region, estimateStart = date)

  # figuring out when estimation ends i.e. applying the delays
  estimateDatesDf <- caseData %>%
    filter(
      !(is.na(value))
    ) %>%
    group_by(region, countryIso3, source, data_type) %>%
    top_n(n = 1, date) %>%
    arrange(countryIso3, region) %>%
    left_join(delays, by = "data_type") %>%
    ungroup() %>%
    transmute(
      countryIso3 = countryIso3, region = region, data_type = data_type, estimateEnd = date - delay) %>%
    left_join(estimateStartDates, by = c("countryIso3", "region"))

  estimatesDates <- list()

  for (iCountry in unique(estimateDatesDf$countryIso3)) {
    tmpCountry <- filter(estimateDatesDf, countryIso3 == iCountry)
    for (iRegion in unique(tmpCountry$region)) {
      tmpRegion <- filter(tmpCountry, region == iRegion)
      tmpListEnd <- tmpRegion$estimateEnd
      names(tmpListEnd) <- tmpRegion$data_type
      tmpListStart <- tmpRegion$estimateStart
      names(tmpListStart) <- tmpRegion$data_type
      estimatesDates[[iCountry]][[iRegion]] <- list(start = tmpListStart, end = tmpListEnd)
    }
  }
  return(estimatesDates)
}

# smooth time series with LOESS method
getLOESSCases <- function(dates, count_data, days_incl = 21, degree = 1, truncation = 0) {

  if (truncation != 0) {
    dates <- dates[1:(length(dates) - truncation)]
    count_data <- count_data[1:(length(count_data) - truncation)]
  }
  
  n_points <- length(unique(dates))
  sel_span <- days_incl / n_points
  
  n_pad <- round(length(count_data) * sel_span * 0.5)

  c_data <- data.frame(value = c(rep(0, n_pad), count_data),
                       date_num = c(seq(as.numeric(dates[1]) - n_pad, as.numeric(dates[1]) - 1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = sel_span, degree = degree)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] <- 0
  raw_smoothed_counts <- smoothed[(n_pad + 1):length(smoothed)]
  normalized_smoothed_counts <-
    raw_smoothed_counts * sum(count_data, na.rm = T) / sum(raw_smoothed_counts, na.rm = T)

  if (truncation != 0) {
    normalized_smoothed_counts <- append(normalized_smoothed_counts, rep(NA, truncation))
  }
  return(normalized_smoothed_counts)
}

mapLabels <- function(shapeFileData, mainLabel = "cases14d") {
  if (mainLabel == "cases14d") {
    labelOrder <- c("cases14d", "re")
  } else (
    labelOrder <- c("re", "cases14d")
  )

  mapLabels <- as_tibble(shapeFileData) %>%
    transmute(
      name = str_c("<strong>", NAME, "</strong>"),
      cases14d = if_else(is.na(cases14d),
        "<br>No data available",
        str_c("<br>", round(cases14d, 3), " cases / 100'000 / 14d (", dateCases, ")")),
      re = if_else(is.na(median_R_mean),
        "<br>No R<sub>e</sub> estimate available",
        str_c("<br>R<sub>e</sub>: ", round(median_R_mean, 3), " ",
        "(", round(median_R_lowHPD, 3), " - ", round(median_R_highHPD, 3), ") (", dateEstimates, ")" ))
    ) %>%
    transmute(
      label = str_c(name, .data[[labelOrder[1]]], .data[[labelOrder[2]]])) %>%
    .$label %>%
    lapply(htmltools::HTML)
  return(mapLabels)
}

addPolygonLayer <- function(map, shapeFile, fillColor, group, labels, options = pathOptions(), layerId = NULL) {
  map <- map %>%
    addPolygons(
      layerId = layerId,
      data = shapeFile,
      fillColor = fillColor,
      weight = 0,
      opacity = 0,
      color = "transparent",
      dashArray = "",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 1,
        opacity = 1,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      group = group,
      options = options)

  return(map)
}

regionCheckboxInput <- function(checkboxGroupId, label, choices, selected, zoomLabel) {
  out <- list()

  header <- glue::glue(
    "<div id='{checkboxGroupId}' class='shiny-input-checkboxgroup shiny-input-container shiny-bound-input'>
      <label class='control-label' for='{checkboxGroupId}'>{label}</label>
      <div class='shiny-options-group'>")


  checkboxes <- list()
  for (i in seq_along(choices)) {
    checkboxes[[i]] <- glue::glue(
      "<div class='checkbox'>",
        "<label>",
            "<input type='checkbox' name='{checkboxGroupId}' value='{choiceValue}' {checked}>",
              "<span>{choiceName}",
              "<a id='zoom{choiceValue}' href='#' class='action-button shiny-bound-input' style='display: inline'>{zoomLabel}</a>",
              "</span>",
        "</label>",
      "</div>",
      checked = if_else(choices[i] %in% selected, "checked='checked'", ""),
      choiceValue = choices[i],
      choiceName = names(choices)[i])
  }

  return(HTML(str_c(header, str_c(checkboxes, collapse = ""), "</div></div>")))   
}

divergentColorPal <- function(palette, domain, midpoint, na.color = "#808080", alpha = FALSE, reverse = FALSE) {
  rng <- NULL
  if (length(domain) > 0) {
    rng <- range(domain, na.rm = TRUE)
    if (!all(is.finite(rng))) {
      stop("Wasn't able to determine range of domain")
    }
  }

  pf <- leaflet:::safePaletteFunc(palette, na.color, alpha)

  leaflet:::withColorAttr("numeric", list(na.color = na.color), function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(pf(x))
    }

    if (is.null(rng)) rng <- range(x, na.rm = TRUE)

    rescaled <- scales::rescale_mid(x, from = rng, mid = midpoint)
    rescaled[rescaled > 1] <- 1
    if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE))
      warning("Some values were outside the color scale and will be treated as NA")

    if (reverse) {
      rescaled <- 1 - rescaled
    }
    pf(rescaled)
  })
}

casesLegendLabels <- function(type, cuts) {
  out <- format(cuts, scientific = FALSE, big.mark = ",")
  out[length(out)] <- str_c("≥", out[length(out)])
  return(out)
}

rValueBox <- function(rEstimate, text, icon, popoverId, popoverTitle, popoverText) {
 
  rEstimateText <- rEstimate %>%
      glue::glue_data(
        "<h3 style=margin-bottom:0px>{round(mean, 3)} ({round(low, 3)} - {round(high, 3)})</h3><p style=margin-top:0px>{country} ({date})</p>"
      )
  if (length(rEstimateText) > 3) {
    rEstimateText <- c(rEstimateText[1:3], "<h3>...</h3>")
  }
  rEstimateText <- rEstimateText %>%
    str_c(collapse = "")

  rValueBox <- tagList(
    HTML(
      glue::glue(
        "<div class='small-box bg-blue'>
          <div class='inner'>
            <p>
              {text} <i class='fa fa-exclamation-circle'></i>
            </p>
            {rEstimateText}
           
          </div>
          <div class = 'icon-large'>
            {icon}
          </div>
        </div>"
      )
    ),
    bsPopover(popoverId, popoverTitle,
      popoverText,
      placement = "bottom", trigger = "hover",
      options = NULL)
    
  )

  return(rValueBox)
}
