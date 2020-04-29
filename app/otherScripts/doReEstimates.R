library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library("reshape")
library("plyr")
library("utils")
library("cbsodataR")
library("tidyverse")


### Apply EpiEstim R estimation method to 'incidenceData' timeseries with 'dates' the dates associated
##
## 'estimateOffsetting' is the number of days the estimates are to be shifted towards the past (to account for delay between infection and testing/hospitalization/death..)
## 'ledtTruncation' is the number of days of estimates that should be ignored at the start of the time series
## 'method' takes value either 'Cori' or  'WallingaTeunis'. 'Cori' is the classic EpiEstim R(t) method, 'WallingaTeunis' is the method by Wallinga and Teunis (also implemented in EpiEstim)
## 'minimumCumul' is the minimum cumulative count the incidence data needs to reach before the first Re estimate is attempted (if too low, EpiEstim can crash)
## 'windowLength' is the size of the sliding window used in EpiEstim
## 'mean_si' and 'std_si' are the mean and SD of the serial interval distribution used by EpiEstim
estimateRe <- function(dates, incidenceData, estimateOffsetting = 10, rightTruncation=0, leftTruncation = 5, method="Cori", variationType= "slidingWindow", interval_ends= c("2020-03-13", "2020-03-16", "2020-03-20"), minimumCumul = 5, windowLength= 4, mean_si = 4.8, std_si  =2.3) {
  
  offset <- 1
  cumulativeIncidence <- 0
  while(cumulativeIncidence < minimumCumul) {
    if(offset > length(incidenceData)) {
      return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
    }
    cumulativeIncidence <- cumulativeIncidence + incidenceData[offset]
    offset <- offset + 1
  }
  
  ## offset needs to be at least two for EpiEstim
  offset <- max(2, offset)
  
  rightBound <- length(incidenceData)- (windowLength -1)
  
  if(rightBound < offset) { ## no valid data point, return empty estimate
    return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
  }
  
  ## generate start and end bounds for Re estimates
  if(variationType == "step") {
    
    interval_end_indices <- sapply(interval_ends, function(x) {which(dates == as.Date(x))[1]})
    
    t_start <- c(offset, na.omit(interval_end_indices) + 1)
    t_end <- c(na.omit(interval_end_indices), length(incidenceData))
    
    if(offset >= length(incidenceData)) {
      return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
    }
    
    while(offset > t_end[1]) { 
      t_start <- t_start[-1]
      t_start[1] <- offset
      t_end <- t_end[-1]
    }
    
    while(t_start[length(t_start)] >= length(incidenceData)) {
      t_end <- t_end[-length(t_end)]
      t_start <- t_start[-length(t_start)]               
    }
    
    outputDates <- dates[t_start[1]:t_end[length(t_end)]]
    
  } else if (variationType == "slidingWindow") {
    t_start <- seq(offset, rightBound)
    t_end <- t_start + windowLength -1
    
    outputDates <- dates[t_end]
  } else {
    print("Unknown time variation.")
    return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
  }
  
  ## offset dates to account for delay between infection and recorded event (testing, hospitalization, death...)
  outputDates <- outputDates - estimateOffsetting
  
  if(method == "Cori") {
    
    R_instantaneous <- estimate_R(incidenceData, 
                                  method="parametric_si", 
                                  config = make_config(list(
                                    mean_si = mean_si, 
                                    std_si = std_si,
                                    t_start = t_start,
                                    t_end = t_end)))
    
  } else if(method == "WallingaTeunis") {
    
    R_instantaneous <- wallinga_teunis(incidenceData,
                                       method="parametric_si",
                                       config = list(
                                         mean_si = mean_si, std_si = std_si,
                                         t_start = t_start,
                                         t_end = t_end,
                                         n_sim = 10))
  } else {
    print("Unknown estimation method")
    return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
  }
  
  if(variationType == "step") {
    R_mean <- unlist(lapply(1:length(t_start), function(x) {rep(R_instantaneous$R$`Mean(R)`[x], t_end[x]-t_start[x]+1)}))
    R_highHPD <- unlist(lapply(1:length(t_start), function(x) {rep(R_instantaneous$R$`Quantile.0.975(R)`[x], t_end[x]-t_start[x]+1)}))
    R_lowHPD <- unlist(lapply(1:length(t_start), function(x) {rep(R_instantaneous$R$`Quantile.0.025(R)`[x], t_end[x]-t_start[x]+1)}))
  } else {
    R_mean <- R_instantaneous$R$`Mean(R)`
    R_highHPD <- R_instantaneous$R$`Quantile.0.975(R)`
    R_lowHPD <- R_instantaneous$R$`Quantile.0.025(R)`
  }
  
  if(rightTruncation > 0) {
    
    if(rightTruncation >= length(outputDates)) {
      return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
    }
    
    originalLength <- length(outputDates)
    outputDates <- outputDates[-seq(originalLength, by=-1, length.out=rightTruncation)]
    R_mean <- R_mean[-seq(originalLength, by=-1, length.out=rightTruncation)]
    R_highHPD <- R_highHPD[-seq(originalLength, by=-1, length.out=rightTruncation)]
    R_lowHPD <- R_lowHPD[-seq(originalLength, by=-1, length.out=rightTruncation)]
    
  }
  
  if (leftTruncation > 0) {

    if(leftTruncation >= length(outputDates)) {
      return(data.frame(date=c(), variable=c(), value=c(), estimate_type=c()))
    }
    originalLength <- length(outputDates)
    outputDates <- outputDates[-seq(1, leftTruncation)]
    R_mean <- R_mean[-seq(1, leftTruncation)]
    R_highHPD <- R_highHPD[-seq(1, leftTruncation)]
    R_lowHPD <- R_lowHPD[-seq(1, leftTruncation)]
  }
  
  result <- data.frame(date=outputDates,
                       R_mean=R_mean, 
                       R_highHPD=R_highHPD,
                       R_lowHPD=R_lowHPD)
  
  result <- melt(result, id.vars="date")
  colnames(result) <- c("date", "variable", "value")
  result$estimate_type <- paste0(method, "_", variationType)
  
  return(result)
}


doReEstimation <- function(data_subset, slidingWindow=1, methods, variationTypes, interval_ends=c("2020-04-01"), delays, truncations) {
  
  end_result <-  data.frame()
  
  for(method_i in methods) {
    
    for(variation_i in variationTypes) {
      
      incidence_data <- data_subset$value[data_subset$variable == "incidence"]
      dates <- data_subset$date[data_subset$variable == "incidence"]
      
      offsetting <- delays[method_i]
      
      leftTrunc <- truncations$left[method_i]
      rightTrunc <- truncations$right[method_i]
      
      result <- estimateRe(dates=dates, 
                           incidenceData=incidence_data, 
                           windowLength =  slidingWindow,
                           estimateOffsetting = offsetting, 
                           rightTruncation = rightTrunc, 
                           leftTruncation=leftTrunc, 
                           method=method_i,
                           variationType = variation_i,
                           interval_ends = interval_ends)
      
      if(nrow(result) > 0) {
        
        result$region <- unique(data_subset$region)[1]
        result$country <- unique(data_subset$country)[1]
        result$source <- unique(data_subset$source)[1]
        result$data_type <- unique(data_subset$data_type)[1]
        result$replicate <- unique(data_subset$replicate)[1]
        ## need to reorder columns in 'results' dataframe to do the same as in data
        result <- result[,c("date", "region", "country", "source", "data_type","estimate_type", "replicate", "value", "variable")]
        
        end_result <- rbind(end_result ,result)
        
      }
    }
  }
  
  return(end_result)
}

## Perform R(t) estimations with EpiEstim on each 'region' of the data, with each 'method' and on each 'data_type'
## 'region' is the geographical region
## 'data_type' can be 'confirmed' for confirmed cases, 'deaths' for fatalities, 'hospitalized' for hospitalization data directly from hospitals (not via openZH here)
doAllReEstimations <- function(data, slidingWindow=3 ,methods=c("Cori", "WallingaTeunis"), variationTypes=c("step", "slidingWindow"), all_delays, truncations, interval_ends=c("2020-04-01")) {
  
  results_list <- list()
  
  for (source_i in unique(data$source)) {
    
    for(region_i in unique(data$region)) {
      
      for(data_type_i in unique(data$data_type)) {
        
        print(region_i)
        print(data_type_i)
        
        subset_data <- subset(data, region == region_i & source == source_i & data_type == data_type_i)
        
        if(nrow(subset_data) == 0) {
          next
        }
        
        delay_i <- all_delays[[data_type_i]]
        
        for (replicate_i in unique(unique(subset_data$replicate))) {
          
          subset_data_rep <- subset(subset_data, subset_data$replicate == replicate_i)
          
          results_list <- c(results_list, list(doReEstimation(subset_data_rep,
                                                              slidingWindow=slidingWindow, 
                                                              methods=methods,
                                                              variationTypes=variationTypes,
                                                              interval_ends=interval_ends,
                                                              delays=delay_i, 
                                                              truncations=truncations)))
        }
      }
    }
  }

  return(rbind.fill(results_list))
}


#################################
#### Start of the script ########
#################################


####################
###### Input #######
####################

outputDir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data"
pathToSampledInfectDataSave <- file.path(outputDir, paste0("Sampled_infect_data_",Sys.Date(), ".Rdata"))
pathToEstimatesReSave <- file.path(outputDir, paste0("Estimates_Re_",Sys.Date(), ".Rdata"))

### Date input
interval_ends <- c("2020-03-13", "2020-03-16", "2020-03-20")
window <- 3

### Delays applied
all_delays <- list(infection_confirmed=c(Cori=0, WallingaTeunis=-5),
                   infection_deaths=c(Cori=0, WallingaTeunis=-5),
                   infection_hospitalized=c(Cori=0, WallingaTeunis=-5),
                   confirmed=c(Cori=10, WallingaTeunis=5),
                   deaths=c(Cori=20, WallingaTeunis=15),
                   hospitalized=c(Cori=8, WallingaTeunis=3))

truncations <- list(left=c(Cori=5, WallingaTeunis=0),
                    right=c(Cori=0, WallingaTeunis=8))


#############################
load(file=pathToSampledInfectDataSave)


### Run EpiEstim
estimatesRe <- doAllReEstimations(subset(sampledInfectData, variable=="incidence"), 
                                  slidingWindow=window, 
                                  methods="Cori",
                                  all_delays=all_delays, 
                                  truncations=truncations,
                                  interval_ends = interval_ends)

save(estimatesRe, file=pathToEstimatesReSave)
  