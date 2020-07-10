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
    group_by(region, country, source, data_type) %>%
    filter(
      data_type == "Confirmed cases",
      !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
      cumsum(incidence) > 100) %>%
    filter(date == min(date)) %>%
    ungroup() %>%
    select(country, region, estimateStart = date)

  # figuring out when estimation ends i.e. applying the delays
  estimateDatesDf <- caseData %>%
    filter(
      !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
      !(is.na(incidence))
    ) %>%
    group_by(region, country, source, data_type) %>%
    top_n(n = 1, date) %>%
    arrange(country, region) %>%
    left_join(delays, by = "data_type") %>%
    ungroup() %>%
    transmute(
      country = country, region = region, data_type = data_type, estimateEnd = date - delay) %>%
    left_join(estimateStartDates, by = c("country", "region"))

  estimatesDates <- list()

  for (iCountry in unique(estimateDatesDf$country)) {
    tmpCountry <- filter(estimateDatesDf, country == iCountry)
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
