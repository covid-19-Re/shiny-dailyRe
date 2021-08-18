library(tidyverse)
library(lubridate)
library(here)
library(qs)
cat(str_c(Sys.time(), " | summarizing data ...\n"))

source(here("app/utils.R"))

pathToCountryData <- here("app", "data", "countryData")
countryNames <- read_csv(here("app", "data", "continents.csv"), col_types = cols(.default = col_character())) %>%
  dplyr::select(-continent)

allCountries <- str_match(
  string = list.files(path = pathToCountryData, pattern = ".*-Data", recursive = TRUE),
  pattern = "(.*)-.*")[, 2]

source(here::here("app/otherScripts/1_utils_getRawData.R"))

vaccinationDataWorld <- getVaccinationDataOWID(tempFileName = here("app/data/temp/owidVaccinationData.csv")) %>%
  left_join(countryNames, by = "countryIso3")
vaccinationDataCHE <- getVaccinationDataCHE() %>%
  left_join(countryNames, by = "countryIso3")
# only use BAG vaccination data if it exists (safety, since data fetching is undocumented)
if (!is.null(vaccinationDataCHE)) {
  vaccinationDataWorld <- vaccinationDataWorld %>%
    filter(!(countryIso3 %in% c("CHE", "LIE")))
}

vaccinationData <- bind_rows(vaccinationDataWorld, vaccinationDataCHE)

qsave(vaccinationData, file = here("app/data/temp/vaccinationData.qs"))

allData <- list(caseData = list(), estimates = list())
estimatePlotRanges <- list()
updateDataRaw_1 <- list()
pb <- txtProgressBar(min = 0, max = length(allCountries))
pb_i <- 0
for (iCountry in allCountries) {
  iCountryData <- loadCountryData(iCountry, dataDir = pathToCountryData)
  allData$caseData[[iCountry]] <- iCountryData$caseData
  allData$estimates[[iCountry]] <- iCountryData$estimates
  estimatePlotRanges[[iCountry]] <- iCountryData$estimateRanges[[iCountry]]

  updateDataRaw_1[[iCountry]] <- bind_rows(
    iCountryData$updateData,
      vaccinationData %>%
      filter(countryIso3 == iCountry) %>%
      group_by(countryIso3, country, region, source,) %>%
      summarize(
        data_type = "Vaccinations",
        lastData = max(date),
        lastChanged = now(),
        lastChecked = now(),
        .groups = "drop"
      ))

  pb_i <- pb_i + 1
  setTxtProgressBar(pb, pb_i)
}
close(pb)

allData$caseData <- bind_rows(allData$caseData) %>%
  # fix country names
  dplyr::select(-country) %>%
  left_join(countryNames, by = "countryIso3")
allData$estimates <- bind_rows(allData$estimates) %>%
  # fix country names
  dplyr::select(-country) %>%
  left_join(countryNames, by = "countryIso3") %>%
  # remove stringency index if it exists
  filter(data_type != "Stringency Index") %>%
  group_by(countryIso3, data_type) %>%
  filter(
      between(date,
        left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
        right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]])
    ) %>%
  mutate(data_type = as.character(data_type))

qsave(allData, file = here("app/data/temp/allCountryData.qs"))

# get Mobility Data
countriesWithRegions <- allData$caseData %>%
  filter(data_type != "Stringency Index") %>%
  select(-local_infection) %>%
  select(countryIso3, region) %>%
  group_by(countryIso3) %>%
  summarise(nRegions = length(unique(region))) %>%
  filter(nRegions > 1)

allMobilityDataGoogle <- getMobilityDataGoogle(
    tempFile = "app/data/temp/mobilityDataGoogle.csv", tReload = 8 * 60 * 60) %>%
  mutate(data_type = placeCategory %>%
    str_replace_all("_", " ") %>%
    str_to_title(),
    change = change * 100) %>%
  filter(countryIso3 == region | countryIso3 %in% countriesWithRegions$countryIso3) %>%
  select(-placeCategory)
qsave(allMobilityDataGoogle, here("app/data/temp/allMobilityDataGoogle.qs"))

allMobilityDataApple <- getMobilityDataApple(tempFile = "app/data/temp/mobilityDataApple.csv", tReload = 8 * 60 * 60) %>%
  mutate(
    data_type = str_to_title(transportationType),
    change = change * 100) %>%
  select(-percent, -transportationType) %>%
  filter(countryIso3 == region | countryIso3 %in% countriesWithRegions$countryIso3) 
qsave(allMobilityDataApple, here("app/data/temp/allMobilityDataApple.qs"))

# prep Data for app
continents <- read_csv(here("app/data/continents.csv"),
  col_types = cols(.default = col_character()))
countryList <- tibble(
    countryIso3 = unique(allData$estimates$countryIso3)
  ) %>%
  left_join(
    continents,
    by = "countryIso3") %>%
  arrange(continent, country) %>%
  split(f = .$continent) %>%
  lapply(function(df) {
    df <- df %>% distinct()
    selectList <- df$countryIso3
    names(selectList) <- df$country
    return(selectList)
  })
qsave(countryList, file = here("app/data/temp/countryList.qs"))

interventionsData <- read_csv(
  here("../covid19-additionalData/interventions/interventions.csv"),
  col_types = cols(
    .default = col_character(),
    date = col_date(format = ""),
    y = col_double()
  )) %>%
  split(f = .$countryIso3)
qsave(interventionsData, file = here("app/data/temp/interventionsData.qs"))

# map data
worldMapEstimates <- allData$estimates %>%
  filter(
    data_type == "Confirmed cases",
    estimate_type == "Cori_slidingWindow") %>%
  group_by(region) %>%
  filter(date == max(date)) %>%
  dplyr::select(
      ADM0_A3_IS = countryIso3,
      region = region,
      estimate_type,
      data_typeEstimate = data_type,
      dateEstimates = date,
      median_R_mean,
      median_R_highHPD,
      median_R_lowHPD)

worldMapData <-  allData$caseData %>%
  bind_rows() %>%
  ungroup() %>%
  filter(
    data_type == "Confirmed cases",
    #sanitize
    !is.na(date)) %>%
  arrange(countryIso3, region, data_type, date) %>%
  group_by(region) %>%
  mutate(
    sum14days = slider::slide_index_dbl(value, date, sum, .before = lubridate::days(14))
  ) %>%
  ungroup() %>%
  mutate(cases14d = sum14days / populationSize * 100000) %>%
  dplyr::select(
    ADM0_A3_IS = countryIso3,
    region = region,
    sourceCases = source,
    data_typeCases = data_type,
    dateCases = date,
    nCases = value,
    cases14d,
    populationSize) %>%
  group_by(ADM0_A3_IS, region) %>%
  filter(dateCases == max(dateCases)) %>%
  left_join(worldMapEstimates, by = c("ADM0_A3_IS", "region")) %>%
  ungroup() %>%
  distinct()

qsave(worldMapData, file = here("app/data/temp/worldMapData.qs"))

countriesShape <- left_join(
  sf::st_read(here("app/data/geoData/ne_50m_admin_0_countries.shp"), quiet = TRUE),
  filter(worldMapData, region == ADM0_A3_IS),
  by = "ADM0_A3_IS")

qsave(countriesShape, file = here("app/data/temp/countriesShape.qs"))

CHEregionsShape <- sf::st_read(
    here("app/data/geoData/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp"), quiet = TRUE) %>%
  sf::st_transform(sf::st_crs(countriesShape)) %>%
  sf::st_zm() %>%
  left_join(
    tibble(
      region = c(
        "ZH", "BE", "LU", "UR", "SZ",
        "OW", "NW", "GL", "ZG", "FR",
        "SO", "BS", "BL", "SH", "AR",
        "AI", "SG", "GR", "AG", "TG",
        "TI", "VD", "VS", "NE", "GE", "JU"),
      KANTONSNUM = c(
        1:26
      )),
    by = "KANTONSNUM") %>%
  mutate(
    ADM0_A3_IS = "CHE"
  ) %>%
  left_join(
    worldMapData,
    by = c("ADM0_A3_IS", "region")
  )
qsave(CHEregionsShape, file = here("app/data/temp/CHEregionsShape.qs"))

cheCasesLabels <- mapLabels(shapeFileData = CHEregionsShape, mainLabel = "cases14d")
qsave(cheCasesLabels, file = here("app/data/temp/cheCasesLabels.qs"))

cheReLabels <- mapLabels(shapeFileData = CHEregionsShape, mainLabel = "re")
qsave(cheReLabels, file = here("app/data/temp/cheReLabels.qs"))

ZAFregionsShape <- sf::st_read(
    here("app/data/geoData/zaf_admbnda_adm1_2016SADB_OCHA.shp"), quiet = TRUE) %>%
  mutate(
    ADM0_A3_IS = "ZAF",
    region = recode(ADM1_EN, "Nothern Cape" = "Northern Cape"),
    NAME = region
  ) %>%
  left_join(
    worldMapData,
    by = c("ADM0_A3_IS", "region"))
qsave(ZAFregionsShape, file = here("app/data/temp/ZAFregionsShape.qs"))

zafCasesLabels <- mapLabels(shapeFileData = ZAFregionsShape, mainLabel = "cases14d")
qsave(zafCasesLabels, file = here("app/data/temp/zafCasesLabels.qs"))
zafReLabels <- mapLabels(shapeFileData = ZAFregionsShape, mainLabel = "re")
qsave(zafReLabels, file = here("app/data/temp/zafReLabels.qs"))

# update updateData
sourceInfo <- read_csv(here("app/data/dataSources.csv"),
  col_types = cols(.default = col_character()))

updateDataRaw <- updateDataRaw_1 %>%
  bind_rows() %>%
  bind_rows(
    allMobilityDataApple %>%
      filter(countryIso3 %in% names(updateDataRaw)) %>%
      group_by(countryIso3, region) %>%
      summarise(
        lastData = max(date),
        lastChanged = lastData,
        .groups = "drop") %>%
      mutate(
        source = "Apple",
        data_type = "Apple Mobility Data", lastChecked = now())
  ) %>%
  bind_rows(
    allMobilityDataGoogle %>%
      filter(countryIso3 %in% names(updateDataRaw)) %>%
      group_by(countryIso3, region) %>%
      summarise(
        lastData = max(date),
        lastChanged = lastData,
        .groups = "drop") %>%
      mutate(
        source = "Google",
        data_type = "Google Mobility Data",
        lastChecked = now())
  ) %>%
  ungroup() %>%
  select(-country) %>%
  left_join(dplyr::select(continents, countryIso3, country), by = "countryIso3") %>%
  split(~countryIso3)

nCountries <- length(updateDataRaw)

dataSources <- updateDataRaw %>%
  bind_rows() %>%
  ungroup() %>%
  dplyr::select(countryIso3, country, source, data_type, lastData) %>%
  filter(
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths",
      "Excess deaths", "Stringency Index", "Vaccinations", "Apple Mobility Data", "Google Mobility Data")
  ) %>%
  left_join(sourceInfo, by = "source") %>%
  group_by(source, sourceLong, url) %>%
  dplyr::summarize(
    countries = case_when(
      length(unique(country)) == nCountries ~ "all countries",
      length(unique(country)) > 5 ~ "other countries",
      TRUE ~ str_c(unique(country), collapse = ", ")),
    data_type = str_c(as.character(unique(data_type)), collapse = ", "),
    .groups = "drop") %>%
  mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
  dplyr::select("Source" = source, "Description" = sourceLong,
    "Countries" = countries, "Data types" = data_type, "URL" = url)

qsave(dataSources, file = here("app/data/temp/dataSources.qs"))
qsave(updateDataRaw, file = here("app/data/temp/updateDataRaw.qs"))

# send notifications
if (file.exists(here("app/otherScripts/sendNotifications.txt")) &
    file.exists(here("app/otherScripts/notificationsToSend.txt"))) {

  source(here("app/otherScripts/utils.R"))
  countries <- scan(here("app/otherScripts/notificationsToSend.txt"), what = "character")
  urls <- scan(here("app/otherScripts/slackWebhook.txt"), what = "character")

  for (country in countries) {
    sendSlackNotification(
      country = country,
      event = "updateDone",
      url = urls[1],
      webhookUrl = urls[2]
    )
  }
  cat(str_c(Sys.time(), " | Slack notifications sent.\n"))
  system(str_c("rm ", here("app/otherScripts/notificationsToSend.txt")))
}

cat(str_c(Sys.time(), " | summarizing data done.\n"))
