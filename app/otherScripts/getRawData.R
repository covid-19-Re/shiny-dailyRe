library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library("reshape")
library("plyr")
library("utils")
library("cbsodataR")
library("tidyverse")


###############################################
################ Utilities ####################
###############################################

## Get incidence data from cumulative counts data and restructure dataframe to long format
## Any data after 'stoppingDate' is excluded
meltCumulativeData <- function(rawData, dataType, dataSource, country="CH", nameDateCol="Date", stoppingDate = (Sys.Date() -1)) {
  
  cumulData <- rawData
  cumulData$Date <- ymd(cumulData$Date)
  
  cumulData <- cumulData[cumulData$Date <= stoppingDate, ]

  cumulData <- melt(cumulData, id.vars=nameDateCol)
  colnames(cumulData) <- c("date", "region", "value")
  
  cumulData <- rbind.fill(lapply(unique(cumulData$region), function(reg) {curateLongTimeSeries(subset(cumulData, region==reg), isIncidenceData=F) }))
  
  cumulData$data_type <- dataType
  cumulData$variable <- "cumul"
  cumulData$country <- country
  cumulData$source <- dataSource
  
  cumulData <- cumulData[,c("date", "region", "country", "source", "data_type", "value", "variable")]
  
  incidenceData <- rbind.fill(lapply(unique(cumulData$region), function(reg){ incidenceSeries <- subset(cumulData, region == reg);
                                                                              incidence <- diff(incidenceSeries$value);
                                                                              incidenceSeries <- incidenceSeries[-1,]
                                                                              incidenceSeries$value <- incidence
                                                                              return(incidenceSeries); }))
  incidenceData$variable <- rep("incidence", nrow(incidenceData))
  
  return(rbind(cumulData, incidenceData))
}

## Prepare time series to be compatible with EpiEstim
curateLongTimeSeries <- function(data, isIncidenceData=T){
  
  ## Remove missing data at beginning of series
  while(nrow(data) > 0 & is.na(data$value[1])) {
    data <- data[-1,]
    if(nrow(data) == 0) {
      return(data.frame())
    }
  }
  
  ## Remove missing data at the end of the series
  while(nrow(data) > 0 & is.na(data$value[nrow(data)])) {
    data <- data[-nrow(data),]
    if(nrow(data) == 0) {
      return(data.frame())
    }
  }
  
  if(isIncidenceData == T) { # incidence time series
    
    ## Replace missing data in rest of series by zeroes (required for using EpiEstim)
    data[is.na(data$value),"value"] <- 0
    
  } else { # cumulative counts time series
    
    ## Replace missing values by the previous day's value
    for(i in 2:nrow(data)) {
      if(is.na(data[i, "value"])) {
        data[i, "value"] <- data[i-1, "value"]
      }
    }
    
  }
  
  return(data)
}

getCountryData <- function(countries, data = getDataECDC()){
  subset_data = data %>%
    filter(country %in% countries)
  return(subset_data)
}

########################################
############ Data fetching #############
########################################

##### Swiss Data #######

## Fetch data from openZH via Daniel Probst's repo
getSwissDataFromOpenZH <- function(stopAfter = (Sys.Date() -1)) {
  
  countTypes <- list("confirmed", "deaths")
  typeLabels <- list("cases", "fatalities")

  names(countTypes) <- typeLabels
  names(typeLabels) <- countTypes
  
  baseUrl <-  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/"
  
  data <- data.frame(date=c(), region=c(), country=c(), value=c(), data_type=c(), variable=c(), estimate_type=c())
  
  for(typeLabel in typeLabels) {
    
    cumulFileUrl <- paste0(baseUrl, "covid19_",typeLabel,"_switzerland_openzh.csv")
    # cumulFileUrl <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data/openZH_daenuprobst/covid19_cases_switzerland_openzh.csv" # Fix while raw.github is down 
    cumulData <- read.csv(cumulFileUrl)
    data <- rbind(data, meltCumulativeData(cumulData,  dataType=countTypes[[typeLabel]], dataSource="openZH", country="CH", stoppingDate = stopAfter))
  }
  
  return(data)
}

## Include hospitalization counts from local csv files
getHospitalData <- function(region="CH", basePathToCSV="./data/Hospital_cases_") {
  filePath <- paste0(basePathToCSV, region, ".csv")
  cumData <- read_csv(filePath)
  cumData <- cumData[,c(1,3)]
  return(meltCumulativeData(cumData, dataType="hospitalized", country="CH", dataSource="BAG"))
}

## Combine openZH data with hospitalization data
getAllSwissData <- function(stoppingAfter = (Sys.Date() -1), pathToHospData, regions=c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH")){
  openZHData <- getSwissDataFromOpenZH(stopAfter=stoppingAfter)
  hospitalData <- rbind(getHospitalData("CH", pathToHospData)) 
  swissData <- subset(rbind(openZHData, hospitalData), region %in% regions)
  return(swissData)
}

##### ECDC Confirmed Case Data #########################
# Differences to John Hopkins: 
# don't need to clean for NA, because there are none
# Dates are in dmy instead of mdy


getLongECDCData <- function(countries = NULL){
  urlfile="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  
  world_data <- read_csv(urlfile)
  longData <- world_data %>% 
    select(c(date = 'dateRep', 
             region = 'countriesAndTerritories',
             country='countriesAndTerritories',
             incidence = 'cases', deaths = 'deaths')) %>%
    mutate(date = dmy(date), source='ECDC') %>%
    pivot_longer(cols = c(incidence, deaths), names_to = "data_type") %>%
    mutate(variable = "incidence")
  
  cumulData <- longData %>%
    group_by(region,country,source, data_type) %>%
    arrange(date) %>%
    mutate(value = cumsum(value), variable = "cumul") %>%
    arrange(region)
  
  longData = rbind.fill(longData, cumulData)
  
  if(!is.null(countries)){
    longData <- getCountryData(countries, data = longData)
  }
  
  longData[longData$value < 0, "value"] = 0
  return(longData)
}

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

dataCHHospitalPath <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data/Hospital_cases_"

outputDir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data"


##### Pull data

### just an example here with keeping all the data from different sources/countries in one dataframe and saving into one file
# rawData <- rbind(getAllSwissData(pathToHospData = dataCHHospitalPath), getLongECDCData())
rawData <- rbind(getAllSwissData(pathToHospData = dataCHHospitalPath))

pathToRawDataSave <- file.path(outputDir, paste0("Raw_data_",Sys.Date(), ".Rdata"))
save(rawData, file=pathToRawDataSave)
