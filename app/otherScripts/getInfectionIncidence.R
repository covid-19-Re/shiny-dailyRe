print(paste("starting getInfectionIncidence.R:", Sys.time()))

library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library("reshape")
library("plyr")
library("utils")
#library("cbsodataR")
library("tidyverse")
library("here")


###### Utilities #########

#### Build empirical CDF from draws summing samples from two gamma distributions
make_empirical_cdf <- function(shape, scale, numberOfSamples=1E6) {
  draws <- round(rgamma(numberOfSamples, shape=shape[1], scale=scale[1]) + rgamma(numberOfSamples, shape=shape[2], scale=scale[2]))
  return(Vectorize(ecdf(draws)))
}



#### Randomly sample infection dates from the incidence timeseries using two gamma distributions
### One gamma-distributed waiting time is the incubation period
### The second one is the period between symptom onset and report (of positive test, death, hospitalization...)
### For each incidence time series 'numberOfReplicates' replicates are drawn
drawInfectionDates <- function(data_subset, numberOfReplicates=100, shapeIncubation, scaleIncubation, shapeOnsetToCount,scaleOnsetToCount) {
  
  
  ### Obtain a pdf for the sum of the gamma distributions for incubation and delay from symptoms onset to count
  Fhat <- make_empirical_cdf(shape=c(shapeIncubation, shapeOnsetToCount), scale=c(scaleIncubation, scaleOnsetToCount))
  
  ### Initialize variables 
  lastInfectionDate <- as.Date("1900-01-01") # start with low date
  partial_results <- list()
  
  
  for(replicateNum in 1:numberOfReplicates){
    
    infectionDates <- c()
    for(dateTest in data_subset$date) {
      
      ## for each date in the time series, if incidence on that day is > 0
      ## draw 'incidenceOnDay' delays between infection and count, with 'incidenceOnDay' the daily incidence
      
      incidenceOnDay <- data_subset$value[data_subset$date == dateTest]
      
      if(!is.na(incidenceOnDay) & incidenceOnDay > 0) {
        sampledInfectionToCountDelay <- round(rgamma(incidenceOnDay, shape=shapeOnsetToCount, scale=scaleOnsetToCount) + rgamma(incidenceOnDay, shape=shapeIncubation, scale=scaleIncubation) )
        
        drawnInfectionTimes <- dateTest - sampledInfectionToCountDelay 
        infectionDates <- c(infectionDates, drawnInfectionTimes)
      }
    }
    
    if(length(infectionDates) == 0){
      return(data.frame())
    }

    infectionDates <- as_date(infectionDates)
    
    ### keep track of the most recent date an infection has been sampled, across all replicates
    lastInfectionDate <- max(lastInfectionDate, max(infectionDates))
    
    ### build a vector of consecutive dates between first and last day an infection was sampled
    allInfectionDates <- seq(min(infectionDates), max(infectionDates), by="days")
    
    lastDayTesting <- max(data_subset$date)
    infectionCount <- c()
    trueInfectionCount <- c()
    
    ### account for the yet-to-be-sampled infections happening on each day
    ### the closer to the present, the more likely it is that an infection has not been reported yet.
    for(i in 1:length(allInfectionDates)) {
      infectionCount[i] <- sum(infectionDates == allInfectionDates[i])
      windowToReport <- as.numeric(lastDayTesting - allInfectionDates[i])
      
      ## Fhat(windowToReport) is the probability that an infection is sampled before 'windowToReport' days
      trueInfectionCount[i] <- round(infectionCount[i] * 1/Fhat(windowToReport)) 
    }
    
    results <- list(date=allInfectionDates, infections=trueInfectionCount)
    
    partial_results <- c( partial_results, list(results))
    
  }
  
  ## Now we need to extend the time series so that they all end on the same day.
  ## Thus we need to add trailing zeroes to the true infection counts that end earlier
  results_list <- list()
  
  for(replicateNum in 1:numberOfReplicates){
    
    infectionDates <-   partial_results[[replicateNum]]$date
    infectionTimeSeries <- partial_results[[replicateNum]]$infections
    lastDayWithSamples <- max(infectionDates)
    
    
    if(lastDayWithSamples < lastInfectionDate) {
      infectionDates <- c(infectionDates, seq(lastDayWithSamples + 1, lastInfectionDate, by="days"))
      infectionTimeSeries <- c(infectionTimeSeries, rep(0, lastInfectionDate - lastDayWithSamples ))
    }
    
    
    #### prepare dataframe for this particular replicate before combining all replicates in a single dataframe
    region <- rep(unique(data_subset$region)[1], length(infectionDates))
    country <- rep(unique(data_subset$country)[1], length(infectionDates))
    source <- rep(unique(data_subset$source)[1], length(infectionDates))
    
    data_type_name <- rep(paste0("infection_", unique(data_subset$data_type)[1]), length(infectionDates))
    variable <- rep("incidence", length(infectionDates))
    replicate <- rep(replicateNum, length(infectionDates))
    
    infectionsDF <- data.frame(date=infectionDates,
                               region=region, 
                               country=country,
                               source=source,
                               data_type=data_type_name,
                               replicate=replicate,
                               value=infectionTimeSeries,
                               variable=variable,
                               stringsAsFactors = F)
    
    results_list <- c(results_list, list(infectionsDF))
  }
  
  return(rbind.fill(results_list))
}


#### Apply drawInfectionDates to the entire dataset
drawAllInfectionDates <- function(data, data_type="confirmed", numberOfReplicates=100 , shapeIncubation, scaleIncubation, shapeOnsetToCount, scaleOnsetToCount) {

  results <- list()
  
  for(count_type_i in data_type) {
    
    for(source_i in unique(data$source)) {
      results_list <- lapply(unique(data$region), 
                             function(x){
                               subset_data <- subset(data, region==x & source == source_i & data_type == count_type_i & variable=="incidence");
                               
                               if(nrow(subset_data) == 0) {return(data.frame())}
                               
                               drawInfectionDates(subset_data, numberOfReplicates, 
                                                  shapeIncubation, scaleIncubation, 
                                                  shapeOnsetToCount[[count_type_i]], 
                                                  scaleOnsetToCount[[count_type_i]]) 
                               
                             })
      results <- c(results, results_list)
    }
  }
  
  return(rbind.fill(results))
}


#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

outputDir <- here("app/data")
pathToRawDataSave <- file.path(outputDir, "Raw_data.Rdata")
pathToSampledInfectDataSave <- file.path(outputDir, "Sampled_infect_data.Rdata")
pathToEURawDataSave <- file.path(outputDir, "EU_Raw_data.Rdata")
pathToEUSampledInfectDataSave <- file.path(outputDir, "EU_Sampled_infect_data.Rdata")

### Waiting time distributions ##
## hardcoded for now, but to be taken outside of script
# incubation: mean = 5.3, sd =3.2 (Linton et al., best gamma distr fit)
meanIncubation <- 5.3
sdIncubation <- 3.2

# onset to test: data from BL canton
meanOnsetToTest <- 5.6
sdOnsetToTest <- 4.2

# onset to hospitalization report: pooled CH data from FOPH (17/04/20 update)
meanOnsetToHosp <- 6.6
sdOnsetToHosp <- 5.1

# onset to death: mean =15.0 sd=6.9 (Linton et al. best gamma distr fit)
meanOnsetToDeath <- 15.0
sdOnsetToDeath <- 6.9

### gamma distribution parameters for incubation period
shapeIncubation <- meanIncubation^2/(sdIncubation^2)
scaleIncubation <- (sdIncubation^2)/meanIncubation

meanOnsetToCount <- c("confirmed"= meanOnsetToTest, "deaths" = meanOnsetToDeath, "hospitalized" = meanOnsetToHosp,
                      "excess_deaths" = meanOnsetToDeath)
sdOnsetToCount <- c("confirmed"= sdOnsetToTest, "deaths" = sdOnsetToDeath, "hospitalized" = sdOnsetToHosp,
                    "excess_deaths" = sdOnsetToDeath)

### parameters for gamma distribution between symptom onset and report
shapeOnsetToCount <- meanOnsetToCount^2/(sdOnsetToCount^2)
scaleOnsetToCount <- (sdOnsetToCount^2)/meanOnsetToCount

#####
replicates <- 100

###############

load(file=pathToRawDataSave)

### Sample infection dates
sampledInfectData <- drawAllInfectionDates(rawData, data_type=c("confirmed", "deaths", "hospitalized"), 
                                              numberOfReplicates = replicates, 
                                              shapeIncubation = shapeIncubation, scaleIncubation = scaleIncubation,
                                              shapeOnsetToCount=shapeOnsetToCount, scaleOnsetToCount=scaleOnsetToCount)

save(sampledInfectData, file=pathToSampledInfectDataSave)

###############

load(file=pathToEURawDataSave)

### Sample infection dates
EUsampledInfectData <- drawAllInfectionDates(EUrawData, data_type=c("confirmed", "deaths", "hospitalized", "excess_deaths"), 
                                           numberOfReplicates = replicates, 
                                           shapeIncubation = shapeIncubation, scaleIncubation = scaleIncubation,
                                           shapeOnsetToCount=shapeOnsetToCount, scaleOnsetToCount=scaleOnsetToCount)

save(EUsampledInfectData, file=pathToEUSampledInfectDataSave)

###############

print(paste("Done getInfectionIncidence.R: ", Sys.time()))
