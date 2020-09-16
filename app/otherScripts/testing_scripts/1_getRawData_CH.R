
library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library("reshape")
library("plyr")
library("utils")
library("cbsodataR")
library("tidyverse")
library("here")

source(here::here("app/otherScripts/1_utils_getRawData.R"))

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
BAG_data_dir_Git <- here::here("../ch-hospital-data/data/CH")

dataDir <- here::here("app/data/exploration")

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
  dplyr::select(-1) %>% 
  dplyr::rename(date = Datum, positive = 'Positive Tests', negative = 'Negative Tests') %>% 
  mutate(total = positive + negative) %>% 
  pivot_longer(cols = -1, names_to = "result")

test_normalisation <- tests %>% filter(result == "total") %>%
  mutate(value = mean(value, na.rm = T)/value) %>% 
  dplyr::select(date, value)



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
  
  write_csv(CHrawData, file.path(dataDir, paste0("Raw_data_CH_", format(recent_dates[i], "%Y-%m-%d") ,".csv")))
  
  test_normalisation_i <- test_normalisation %>%  
    complete(date = CHrawData$date, fill = list(value = NA) ) %>%
    filter(date %in% CHrawData$date)
  
  CHrawData_corrected <- CHrawData %>% 
    mutate(value = value * test_normalisation_i$value) %>%
    filter(!is.na(value))

  
  CHrawData <- CHrawData  %>% mutate(type = "raw")
  CHrawData_corrected <- CHrawData_corrected  %>% mutate(type = "corrected")
  
  ch_data <- bind_rows(CHrawData, CHrawData_corrected)
   
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
