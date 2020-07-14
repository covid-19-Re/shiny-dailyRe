# helpers

dataUpdatesTable <- function(
  latestData,
  lastCheck,
  dateFormat = "%Y-%m-%d",
  showDataType = FALSE) {

  latestData <- latestData %>%
    group_by(country, source) %>%
    slice(1L)
  showCountry <- length(unique(latestData$country)) > 1

  outList <- list("<table style=\"width:100%\">")
  for (i in 1:dim(latestData)[1]) {
    if (i == 1) {
      printCountry <- showCountry
    } else {
      printCountry <- (showCountry & latestData[i - 1, ]$country != latestData[i, ]$country)
    }

    if (printCountry) {
      countryString <- str_c("<tr><td colspan=\"3\">", latestData[i, ]$country, "</td></tr>")
    } else {
      countryString <- ""
    }

    sourceString <- str_c("<td style = \"font-weight: normal;\">", latestData[i, ]$source, "</td>")
    if (showCountry) {
      sourceString <- str_c("<td>&nbsp;&nbsp;</td>", sourceString)
    }

    outList[[i + 1]] <- str_c(
      countryString,
      "<tr>",
        sourceString,
        "<td style = \"font-weight: normal;font-style: italic;\">", format(latestData[i, ]$date, dateFormat), "</td>",
      "</tr>")
  }
  outList[[i + 2]] <- "</table>"
  out <- str_c(outList, collapse = "")
  out <- str_c(out, "<small style=\"font-weight: normal\">", lastCheck, "</small>")
  return(out)
}

delaysDf <- tibble(
    data_type = factor(
      c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths"),
      levels = c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")),
    delay = c(8, 10, 18, 30))

estimateRanges <- function(
  caseData,
  minConfirmedCases = 100,
  delays = delaysDf) {

  estimateStartDates <- caseData %>%
    group_by(countryIso3, region, source, data_type) %>%
    arrange(countryIso3, region, source, data_type, date) %>%
    filter(
      data_type == "Confirmed cases",
      cumsum(incidence) > 100) %>%
    filter(date == min(date)) %>%
    ungroup() %>%
    select(countryIso3, region, estimateStart = date)

  # figuring out when estimation ends i.e. applying the delays
  estimateDatesDf <- caseData %>%
    filter(
      !(is.na(incidence))
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
