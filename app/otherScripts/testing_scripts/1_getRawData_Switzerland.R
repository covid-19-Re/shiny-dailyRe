startTime <- Sys.time()
saveRDS(startTime, file = "ScriptStartTime.rds")
cat(paste("###", startTime, "- starting 1_getRawData_Switzerland.R", "\n"))

library("lubridate")
library("readr")
library("gridExtra")
library("plyr")
library("utils")
library("cbsodataR")
library("tidyverse")
library("here")

###############################################
################ Utilities ####################
###############################################

## Get incidence data from cumulative counts data and restructure dataframe to long format
## Any data after "stoppingDate" is excluded
meltCumulativeData <- function(
  rawData,
  dataType,
  dataSource,
  country = "CH",
  nameDateCol = "Date",
  stoppingDate = (Sys.Date() - 1)) {

  cumulData <- rawData
  cumulData$Date <- ymd(cumulData$Date, locale = "en_GB.UTF-8")
  cumulData <- cumulData[cumulData$Date <= stoppingDate, ]
  cumulData <- melt(cumulData, id.vars = nameDateCol)
  colnames(cumulData) <- c("date", "region", "value")

  cumulData <- bind_rows(lapply(
    unique(cumulData$region),
    function(reg) {
      curateLongTimeSeries(subset(cumulData, region == reg), isIncidenceData = F)
    }
  ))

  cumulData$data_type <- dataType
  cumulData$variable <- "cumul"
  cumulData$country <- country
  cumulData$source <- dataSource
  cumulData <- cumulData[, c("date", "region", "country", "source", "data_type", "value", "variable")]

  incidenceData <- bind_rows(lapply(
    unique(cumulData$region), function(reg) {
      incidenceSeries <- subset(cumulData, region == reg);
      incidence <- diff(incidenceSeries$value);
      incidenceSeries <- incidenceSeries[-1, ]
      incidenceSeries$value <- incidence
      return(incidenceSeries);
    }
  ))
  incidenceData$variable <- rep("incidence", nrow(incidenceData))

  return(bind_rows(cumulData, incidenceData))
}

## Prepare time series to be compatible with EpiEstim
curateLongTimeSeries <- function(data, isIncidenceData = TRUE) {
  ## Remove missing data at beginning of series
  while (nrow(data) > 0 & is.na(data$value[1])) {
    data <- data[-1, ]
    if (nrow(data) == 0) {
      return(data.frame())
    }
  }

  ## Remove missing data at the end of the series
  while (nrow(data) > 0 & is.na(data$value[nrow(data)])) {
    data <- data[-nrow(data), ]
    if (nrow(data) == 0) {
      return(data.frame())
    }
  }

  if (isIncidenceData == TRUE) { # incidence time series
    ## Replace missing data in rest of series by zeroes (required for using EpiEstim)
    data[is.na(data$value), "value"] <- 0
  } else { # cumulative counts time series
    ## Replace missing values by the previous days value
    for (i in 2:nrow(data)) {
      if (is.na(data[i, "value"])) {
        data[i, "value"] <- data[i - 1, "value"]
      }
    }
  }
  return(data)
}

getCountryData <- function(countries, data = getDataECDC()) {
  subset_data <- data %>%
    filter(country %in% countries)
  return(subset_data)
}

getCumulData <- function(data) {
  cumulData <- data %>%
    group_by(country, data_type) %>%
    arrange(date) %>%
    mutate(value = cumsum(value), variable = "cumul") %>%
    arrange(country)
  return(cumulData)
}

calcIncidenceData <- function(data) {
  incidence <- diff(data$value)
  data <- data[-1, ]
  data$value <- incidence
  data$variable <- "incidence"
  return(data)
}

########################################
############ Data fetching #############
########################################


getSwissDataFromBAG <- function(path, filename = "incidence_data_CH.csv"){
  filePath <- file.path(path, filename)
  bagData <- read_csv(filePath,
    col_types = cols(
      date = col_date(format = ""),
      region = col_character(),
      country = col_character(),
      source = col_character(),
      data_type = col_character(),
      value = col_double(),
      variable = col_character()))

  bagDataCH <- bagData %>% filter(region == "CH")

  return(bagDataCH)
}

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

dataCHHospitalPath <- here::here("app/data/CH")
BAG_data_dir <- here::here("app", "data", "BAG")
BAG_data_dir_Git <- here::here("../ch-hospital-data/data/CH")

dataDir <- here::here("app/data/temp")

##### Pull data



bagFiles_tests <- list.files(BAG_data_dir,
                       pattern = "*_Time_series_tests.csv",
                       full.names = TRUE,
                       recursive = TRUE)

bagFileDates_tests <- strptime(
  stringr::str_match(bagFiles_tests, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

newestTestFile <- bagFiles_tests[which(bagFileDates_tests == max(bagFileDates_tests))[1]]

tests_file <- read_csv2(newestTestFile, col_names = T)

tests <- tests_file %>%
  select(-1) %>% 
  rename(date = Datum, positive = 'Positive Tests', negative = 'Negative Tests') %>% 
  mutate(total = positive + negative) %>% 
  pivot_longer(cols = -1, names_to = "result")

test_normalisation_rolling <- tests %>% filter(result == "total") %>% 
  mutate(value = zoo::rollmeanr(value, k = 7, fill = NA )) %>%
  select(-result) %>%
  mutate(value  = mean(value, na.rm = T)/value)

test_normalisation <- tests %>% filter(result == "total") %>%
  mutate(value = mean(value, na.rm = T)/value) %>% 
  select(date, value)



### Find latest BAG data file

bagFiles <- list.files(BAG_data_dir,
             pattern = "*FOPH_COVID19_data_extract.csv",
             full.names = TRUE,
             recursive = TRUE)

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

recent_dates <- unique(sort(bagFileDates, decreasing = T))

i <- 1
for(i in 1:length(recent_dates)) {
  ##### Swiss data
  CHrawData <- getSwissDataFromBAG(path = dataCHHospitalPath, filename = paste0("incidence_data_CH_", format(recent_dates[i], "%Y-%m-%d") ,".csv")) %>%
    mutate(
      region = recode(region, "CH" = "Switzerland", "FL" = "Liechtenstein"),
      country = recode(country, "CH" = "Switzerland", "FL" = "Liechtenstein")) %>%
    filter(region == "Switzerland",
           data_type == "confirmed",
           variable == "incidence") %>%
    mutate(
      data_type = factor(
        data_type,
        levels = c("confirmed"),
        labels = c("Confirmed cases")))
  
  ##### Finished pulling data
  
  pathToRawDataSave <- file.path(dataDir, paste0("Raw_data_CH_", format(recent_dates[i], "%Y-%m-%d") ,".rds"))
  
  saveRDS(CHrawData, file = pathToRawDataSave)
  
  # write_csv(CHrawData, file.path(dataDir, paste0("Raw_data_CH_", format(recent_dates[i], "%Y-%m-%d") ,".csv")))
  
  test_normalisation_i <- test_normalisation %>%  
    complete(date = CHrawData$date, fill = list(value = NA) ) %>%
    filter(date %in% CHrawData$date)
  
  test_normalisation_rolling_i <- test_normalisation_rolling %>%  
    complete(date = CHrawData$date, fill = list(value = NA) ) %>%
    filter(date %in% CHrawData$date)
  
  CHrawData_corrected_rolling <- CHrawData %>% 
    mutate(value = value * test_normalisation_rolling_i$value) %>%
    filter(!is.na(value))
  
  saveRDS(CHrawData_corrected_rolling, file = file.path(dataDir, paste0("Raw_data_CH_corrected_rolling_average_", format(recent_dates[i], "%Y-%m-%d") ,".rds")))
  # write_csv(CHrawData_corrected_rolling, file.path(dataDir, paste0("Raw_data_CH_corrected_rolling_average_", format(recent_dates[i], "%Y-%m-%d") ,".csv")))
  
  CHrawData_corrected <- CHrawData %>% 
    mutate(value = value * test_normalisation_i$value) %>%
    filter(!is.na(value))
  
  pathToRawDataSave_corrected <- file.path(dataDir, paste0("Raw_data_CH_corrected_", format(recent_dates[i], "%Y-%m-%d") ,".rds"))
  
  saveRDS(CHrawData_corrected, file = pathToRawDataSave_corrected)
  # write_csv(CHrawData_corrected, file.path(dataDir, paste0("Raw_data_CH_corrected_", format(recent_dates[i], "%Y-%m-%d") ,".csv")))
  
  CHrawData <- CHrawData  %>% mutate(type = "raw")
  CHrawData_corrected <- CHrawData_corrected  %>% mutate(type = "corrected")
  CHrawData_corrected_rolling <- CHrawData_corrected_rolling %>% mutate(type = "corrected_rolling")
  
  ch_data <- bind_rows(CHrawData, CHrawData_corrected, CHrawData_corrected_rolling)
  
  ggplot(ch_data) +
    geom_line(aes(x = date, y = value, colour = type), size= 2 ) + 
    # geom_bar(stat = "identity",position = "dodge", aes(x = date, y = value, fill = type) ) + 
    scale_x_date(limits = c(as.Date("2020-05-01"), Sys.Date()),
                 date_breaks = "4 days",
                 date_labels = '%b\n%d',) +
    coord_cartesian(ylim=c(0,150)) +
    ylab("Incidence") +
    theme_bw()
  
  ggsave(file.path(dataDir, paste0("plot_ch_data_corrected_", format(recent_dates[i], "%Y-%m-%d") ,".png")))
  
  saveRDS(ch_data, file = file.path(dataDir, paste0("CH_data_with_correction_", format(recent_dates[i], "%Y-%m-%d") ,".rds")))
  write_csv(ch_data, file.path(dataDir, paste0("CH_data_with_correction_", format(recent_dates[i], "%Y-%m-%d") ,".csv")))
    
}

cat(paste("###", Sys.time(), "- done 1_getRawData_Switzerland.R", "\n"))
