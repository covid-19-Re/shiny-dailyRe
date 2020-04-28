### Function made to account for missing entries in data
## 
## Makes linear interpolation of missing values
## 
## For dates after 'startDate' if two consecutive reports of cumulative cases are equal, replaces the second one by an interpolation with the entry from the previous and next day
## This is done to account for days when the confirmed case data was not updated.
## It does not work if the data was not updated more than 2 days in a row.
##  
## The function assumes that entry data "cumulDataVector" is either given, 
## or has an NA, for all days of the calendar (no day is skipped entirely)
fillInMissingCumulativeData <- function(dates, cumulDataVector, startDate="2020-03-12"){
  
  if(sum(is.na(cumulDataVector)) > 0 & sum(!is.na(cumulDataVector)) > 1) {
    known_dates <- dates[!is.na(cumulDataVector)]
    cumulDataVector <- na.omit(cumulDataVector)
    
    interpolated_cases <- floor(approx(known_dates, cumulDataVector, xout = dates)$y) 
  } else { # no missing data, no interpolation needed
    interpolated_cases <- cumulDataVector
  }
  
  ## interpolate number of confirmed cases for consecutive equal entries (no reporting on the second day)
  for(i in which(dates == startDate):(length(interpolated_cases) -1)) {
    if(!is.na(interpolated_cases[i]) & !is.na(interpolated_cases[i-1])) {
      if(interpolated_cases[i] == interpolated_cases[i-1]) {
        interpolated_cases[i] <- floor((interpolated_cases[i-1] + interpolated_cases[i+1]) / 2)
      }
    }
  }
  
  return(interpolated_cases)
}

## Get incidence data from cumulative counts data and restructure dataframe
## Any data after 'stoppingDate' is excluded
meltCumulativeData <- function(rawData, dataType, fillInMissingData=F, stoppingDate = (Sys.Date() -1)) {
  
  cumulData <- rawData
  cumulData$Date <- ymd(cumulData$Date)

  cumulData <- cumulData[cumulData$Date <= stoppingDate, ]

  if(fillInMissingData == T) { ### fill in missing incidence data
    cumulData <-cbind(Date=cumulData$Date, as.data.frame(apply(cumulData[,-1, drop=F], 2 , function(x) {fillInMissingCumulativeData(cumulData$Date, x)})))
  }
  
  incidenceData <- as.data.frame(apply(cumulData[,-1, drop=F], 2, function(x) {  incidence <- x - c(0, x[1:(length(x)-1)]) ; return(incidence)}))
  incidenceData <- cbind(Date=cumulData$Date, incidenceData)
  incidenceData <- melt(incidenceData, id.vars="Date")
  colnames(incidenceData) <- c("date", "region", "value")
  incidenceData$data_type <- dataType
  incidenceData$variable <- "incidence"
  incidenceData$estimate_type <- NA
  incidenceData$replicate <- NA
  
  # only done for plotting, but not applied to incidence data
  if(fillInMissingData == F) {
    cumulData <-cbind(Date=cumulData$Date, as.data.frame(apply(cumulData[,-1, drop=F], 2 , function(x) {fillInMissingCumulativeData(cumulData$Date, x)})))
  }
  
  cumulData <- melt(cumulData, id.vars="Date")
  colnames(cumulData) <- c("date", "region", "value")
  cumulData$data_type <- dataType
  cumulData$variable <- "cumul"
  cumulData$estimate_type <- NA
  cumulData$replicate <- NA
  
  return(rbind(cumulData, incidenceData))
}

## Fetch data from openZH via Daniel Probst's repo
getSwissDataFromOpenZH <- function(stopAfter = (Sys.Date() -1)) {
  
  countTypes <- list("confirmed", "deaths")
  typeLabels <- list("cases", "fatalities")

  names(countTypes) <- typeLabels
  names(typeLabels) <- countTypes
  
  baseUrl <-  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/"
  
  data <- data.frame(date=c(), region=c(), value=c(), data_type=c(), variable=c(), estimate_type=c())
  
  for(typeLabel in typeLabels) {
    
    cumulFileUrl <- paste0(baseUrl, "covid19_",typeLabel,"_switzerland_openzh.csv")
    # cumulFileUrl <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data/openZH_daenuprobst/covid19_cases_switzerland_openzh.csv" # Fix while raw.github is down 
    cumulData <- read.csv(cumulFileUrl)
    data <- rbind(data, meltCumulativeData(cumulData,  countTypes[[typeLabel]], stoppingDate = stopAfter))
  }
  
  return(data)
}
