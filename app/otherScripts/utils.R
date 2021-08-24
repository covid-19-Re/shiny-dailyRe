loadCountryData <- function(iso3, dataDir = "data/countryData") {

  allPaths <- list.files(path = dataDir, recursive = TRUE)

  dataPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-Data.rds"))
  if (!is_empty(dataPath)) {
    caseData <- readRDS(file.path(dataDir, dataPath))
    if ("report_plotting" %in% caseData$date_type) {
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
    estimates <- readRDS(file.path(dataDir, estimatesPath)) %>%
      mutate(
        countryIso3 = replace_na(countryIso3, iso3)
      )
  } else {
    estimates <- NULL
  }

  OxCGRTPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-OxCGRT.rds"))
  if (!is_empty(OxCGRTPath)) {

    OxCGRTData <- readRDS(file.path(dataDir, OxCGRTPath)) %>%
      dplyr::select(countryIso3, region, date, matches("\\w\\d_"))
  
  }

  if (!is.null(caseData)) {
    estimateRanges <- estimateRanges(
      filter(caseData, data_type != "Stringency Index"),
      minConfirmedCases = 100,
      delays = delaysDf)
  } else (
    estimateRanges <- NULL
  )

  if (!is.null(caseData)) {
    updateData <- caseData %>%
        group_by(countryIso3, country, region, source, data_type) %>%
        dplyr::summarize(lastData = max(date), .groups = "keep") %>%
        mutate(
          lastChanged = file.mtime(file.path(dataDir, dataPath)),
          lastChecked = file.mtime(file.path(dataDir, dataPath)))
  } else (
    updateData <- NULL
  )

  countryData <- list(
    caseData = caseData,
    estimates = estimates,
    estimateRanges = estimateRanges,
    updateData = updateData)

  return(countryData)
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

cleanEnv <- function(keepObjects, keepSelf = TRUE) {
  envVars <- ls(name = 1)
  if (keepSelf) {
    keepObjects <- c(keepObjects, "cleanEnv", "sendSlackNotification")
  }
  rm(list = setdiff(envVars, keepObjects), pos = 1)
}

sendSlackNotification <- function(country, event, url, eTcompletion, webhookUrl) {
  if (event == "newData") {
    message <- list(
      text = str_c(country, ": new incidence data"),
      blocks = list(list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = str_c(
            Sys.time(), ": *New incidence data for ", country, " is available.*\n",
            "updating ", url, "\n",
            "Estimated time of completion of update: ~ ", eTcompletion)
        )
      ))
    )
  } else if (event == "updateDone") {
    message <- list(
      text = str_c(country, ": new Re estimates calculated"),
      blocks = list(list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = str_c(
            Sys.time(), ": *New Re estimates for ", country, " are available. Will go live 13:30 (Mon-Fri) / 15:30 (Sat/Sun)!*")
        )
      ))
    )
  } else if (event == "dataLive") {
    message <- list(
      text = str_c(country, ": new Re estimates"),
      blocks = list(list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = str_c(
            "<!channel>\n",
            Sys.time(), ": *New Re estimates for ", country, " are live!*\n",
            ":rocket: ", url)
        )
      ))
    )
  } else {
    warning("Invalid Event: No Slack notification sent")
  }

  out <- system(
      str_c(
        "curl -X POST -H 'Content-type: application/json' --data ",
        "'", jsonlite::toJSON(message, flatten = TRUE, auto_unbox = TRUE), "' ",
        webhookUrl
      )
    )

  invisible(out)
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
