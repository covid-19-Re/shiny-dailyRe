# only show package messages when in interactive mode to clean up error logs
  library("lubridate")
  library("fitdistrplus")
  library("EpiEstim")
  library("readxl")
  library("here")
  library("tidyverse")


source(here::here("app/otherScripts/1_utils_getRawData.R"))
source(here::here("app/otherScripts/utils.R"))

args <- c("country" = "CHE")

popData <- qs::qread(file = here("app/data/dev-clusterData/data/popData.qs"))

stringencyIndex <- qs::qread(file = here("app/data/dev-clusterData/data/stringencyIndex.qs"))

interval_ends <- qs::qread(file = here("app/data/dev-clusterData/data/interval_ends.qs"))

incidenceDataFiles <- list.files(path = here("app/data/dev-clusterData/data/incidenceData"))
write.csv(incidenceDataFiles, "dev-test.csv")


for (thisFile in incidenceDataFiles) {
  # Fetch Country Data
  countryData <- getCountryData(
    args["country"],
    ECDCtemp = here::here("app/data/temp/ECDCdata.csv"),
    HMDtemp = here::here("app/data/temp/HMDdata.csv"),
    filename = here("app/data/dev-clusterData/data/incidenceData", thisFile),
    tReload = 30) %>%
    left_join(
      popData,
      by = c("countryIso3", "region")
    ) %>%
    bind_rows(
      mutate(stringencyIndex,
        date_type = if_else(
          args["country"] %in% c("CHE", "DEU", "HKG"), "report_plotting", "report"))
    )

  print(dim(countryData))

  qs::qsave(countryData,
    here("app/data/dev-clusterData/data/countryData",
    str_c(str_sub(thisFile, end = 9), "countryData.qs")
    )
  )
}
