cat(paste("###", Sys.time(), "- starting 2_getInfectionIncidence.R", "\n"))

library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library("reshape")
library("plyr")
library("utils")
library("tidyverse")
library("here")


###### Utilities #########

getLOESSCases <- function(dates, count_data, span=0.25){
  n_pad <- round(length(count_data)*span*0.5)
  c_data <- data.frame(value = c(rep(0, n_pad), count_data), 
                       date_num = c(seq(as.numeric(dates[1])-n_pad, as.numeric(dates[1])-1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = span)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] = 0
  return(smoothed[(n_pad+1):length(smoothed)])
}



#### Build empirical CDF from draws summing samples from two gamma distributions
make_empirical_cdf <- function(shape, scale, numberOfSamples = 1E6) {
  draws <-
    rgamma(numberOfSamples, shape = shape[1], scale = scale[1]) +
    rgamma(numberOfSamples, shape = shape[2], scale = scale[2])
  return(Vectorize(ecdf(draws)))
}

prob_being_observed <- function(discretized_distr, currentIdx, lastObservedIdx) {
  return(sum(discretized_distr[1:(lastObservedIdx - currentIdx + 1)]))
}

discretize_waiting_time_distr <- function(shape, scale, length_out = 250){
  F_h <- make_empirical_cdf(shape, scale)
  f <- Vectorize(function(x){
    if(x < 0) {
      return(0)
    } else if(x < 0.5) {
      return(F_h(0.5))
    } else {
      return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
    }
  })
  x <- 0:length_out
  return(f(x))
}

get_discretized_prob_delay <- function(discretized_distr, date_1, date_2) {
  if(date_1 > date_2) {
    return(0)
  }
  
  return(discretized_distr[as.numeric(date_2 - date_1) + 1]) # +1 because first element is for delay of 0
}


get_original_extended_incidence <- function(original_data, max_delay = 20) {
  extension <- rep(0, times = max_delay)
  return(c(extension, original_data$value ))
}


get_chi_squared <- function(estimated_incidence, original_incidence, discretized_distr, max_delay){
  
  N <- length(original_incidence) - max_delay
  
  internal_sum <- sum(
    sapply(
      ( max_delay + 1 ) : length( original_incidence ), 
      function(index_i) {
        
        expected_reports_day_i <- get_expected_number_of_reports(estimated_incidence, discretized_distr, index_i)
        
        if( expected_reports_day_i == 0 ) {
          return( 0 )
        }
        return(( expected_reports_day_i - original_incidence[index_i])^2 / expected_reports_day_i)
      }
    )
  )
  
  return(internal_sum/N)
}

get_expected_number_of_reports <- function(estimated_incidence, discretized_distr, stop_idx) {
  return(
    sum(
      sapply(1:stop_idx, function(begin_idx) {
        return(discretized_distr[stop_idx - begin_idx  + 1] * estimated_incidence[begin_idx])
      }
      )
    )
  )
}

update_single_estimate_incidence_RL <- function(estimated_incidence, original_incidence, discretized_distr, index_j) {
  
  correcting_factor <- sum(
    sapply(index_j:length(estimated_incidence), function(index_i) {
      if(original_incidence[index_i] == 0) {
        return(0)
      }
      
      ## Can D_i == 0 happen and prob_being_observed is not zero?
      D_i <- get_expected_number_of_reports(estimated_incidence, discretized_distr, index_i)
      
      if(D_i == 0) {
        return(0)
      }
      
      result <- discretized_distr[index_i - index_j  +1] * original_incidence[index_i] / D_i
      return(result)
    })
  )
  
  q_j <- prob_being_observed(discretized_distr, index_j, length(estimated_incidence))
  
  if(q_j == 0) {
    return(0)
  }
  
  updated_value <- estimated_incidence[index_j] * correcting_factor / q_j
  return(updated_value)
} 

update_estimate_incidence_RL <- function(estimated_incidence, original_incidence, discretized_distr) {
  updated_estimate <- sapply(1:length(estimated_incidence), function(index_j){
    update_single_estimate_incidence_RL(estimated_incidence, original_incidence, discretized_distr, index_j)
  })
  return(updated_estimate)
}

iterate_RL <- function(initial_estimate, 
                       original_incidence, 
                       discretized_distr, 
                       threshold_chi_squared = 1, 
                       max_iterations = 20, 
                       max_delay,
                       verbose = F){
  
  current_estimate <- initial_estimate
  
  count <- 1
  while( (chi <- get_chi_squared(current_estimate, original_incidence, discretized_distr, max_delay = max_delay)) > threshold_chi_squared & count <= max_iterations) {
    
    if( verbose ) {
      cat("\t\tStep: ", count, " - Chi squared: ", chi, "\n")
    }
    
    current_estimate <- update_estimate_incidence_RL(current_estimate, original_incidence, discretized_distr)
    count <- count + 1
  }
  
  return(current_estimate)
}

get_infection_incidence_by_deconvolution <- function(   data_subset,
                                                        shapeIncubation,
                                                        scaleIncubation,
                                                        shapeOnsetToCount,
                                                        scaleOnsetToCount,
                                                        min_chi_squared = 0.5,
                                                        maximum_iterations = 30,
                                                        verbose = F,
                                                        absolute_max_reporting_delay = 50) {
  
  
  discretized_distr <- discretize_waiting_time_distr(shape = c(shapeIncubation, shapeOnsetToCount),
                                                     scale = c(scaleIncubation, scaleOnsetToCount),
                                                     length_out = nrow(data_subset) + absolute_max_reporting_delay)
  
  ## TODO this is temporary
  first_guess_delay <- which.max(discretized_distr)[1] - 1
  
  if( verbose ) {
    cat("\tDelay on first guess: ", first_guess_delay, "\n")
  }
  
  minimal_date <- min(data_subset$date) - first_guess_delay
  maximal_date <- max(data_subset$date)
  
  smoothed_incidence_data <- data_subset %>%
    mutate( value = getLOESSCases( dates = date,
                                   count_data = value )) %>%
    complete( date = seq.Date( minimal_date,
                               maximal_date, 
                               by = "days" )) %>%
    mutate( value = if_else(is.na(value), 0, value )) %>%
    mutate( variable = "smoothed")
  
  
  first_guess <- smoothed_incidence_data %>% 
    mutate(date = date - first_guess_delay) %>% 
    complete( date = seq.Date( minimal_date, maximal_date, by = "days" )) %>%
    filter( date >=  minimal_date) %>%
    mutate( value = if_else(is.na(value), 0, value ))
  
  
  final_estimate <- iterate_RL( first_guess$value, 
                                smoothed_incidence_data$value, 
                                discretized_distr = discretized_distr, 
                                threshold_chi_squared = min_chi_squared, 
                                max_iterations = maximum_iterations, 
                                max_delay = first_guess_delay,
                                verbose = verbose )
  
  ## right-truncate trailing zeroes induced by initial shift by 'first_guess_delay'
  deconvolved_dates <- first_guess %>%
    filter( date <= maximal_date - first_guess_delay ) %>%
    .$date
  deconvolved_infections <- final_estimate[ 1 : ( length(final_estimate) - first_guess_delay )]
  
  ## prepare metadata for result dataframe
  data_type_subset <- unique(data_subset$data_type)[1]
  if(data_type_subset %in% c("Hospitalized patients - onset", "Hospitalized patients - admission")) {
    data_type_subset <- "Hospitalized patients"
  }
  data_type_name <- paste0("infection_", data_type_subset)
  
  ## dataframe containing results
  deconvolved_infections <- tibble(
    date = deconvolved_dates,
    region = unique(data_subset$region)[1],
    country = unique(data_subset$country)[1],
    source = unique(data_subset$source)[1],
    data_type = data_type_name,
    replicate = 0,
    value = deconvolved_infections,
    variable = "incidence"
  )
  
  return( deconvolved_infections )
}


### One gamma-distributed waiting time is the incubation period
### The second one is the period between symptom onset and report (of positive test, death, hospitalization...)
get_all_infection_incidence <- function(  data,
                                          data_type,
                                          shapeIncubation,
                                          scaleIncubation,
                                          shapeOnsetToCount,
                                          scaleOnsetToCount,
                                          min_chi_squared = 0.5,
                                          maximum_iterations = 30,
                                          verbose = F){
  results <- list(tibble())
  
  for (count_type_i in data_type) {
    
    cat("Deconvolve infections for data type:", count_type_i, "\n")
    
    for (source_i in unique(data$source)) {
      
      cat("   Data source:", source_i, "\n")
      
      results_list <- lapply(
        unique(data$region),
        function(x) {
          cat("    Region:", x, "\n")
          subset_data <- data %>%
            filter(region == x,
                   source == source_i,
                   data_type == count_type_i,
                   variable == "incidence")
          
          if (nrow(subset_data) == 0) {
            return(tibble())
          }
          
          get_infection_incidence_by_deconvolution(   subset_data,
                                                      shapeIncubation,
                                                      scaleIncubation,
                                                      shapeOnsetToCount[[count_type_i]],
                                                      scaleOnsetToCount[[count_type_i]],
                                                      min_chi_squared,
                                                      maximum_iterations,
                                                      verbose )
          
        })
      
      results <- c(results, results_list)
    }
  }
  
  return(bind_rows(results))
}

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

data_dir <- here::here("app/data/temp")
raw_data_path <- file.path(data_dir, "Raw_data.Rdata")
infect_data_path <- file.path(data_dir, "Deconvolved_infect_data.rds")

##TODO update
##TODO empirical distributions where available
### Waiting time distributions ##
## hardcoded for now, but to be taken outside of script
# incubation: mean = 5.3, sd =3.2 (Linton et al., best gamma distr fit)
meanIncubation <- 5.3
sdIncubation <- 3.2

# onset to test: pooled CH data from BAG (12/05/20 update)
meanOnsetToTest <- 4.5
sdOnsetToTest <- 4.9

# onset to hospitalization report: pooled CH data from BAG (12/05/20 update)
meanOnsetToHosp <- 6.1
sdOnsetToHosp <- 4.7

##TODO reconsider Excess death delay (discretize by week)
# onset to death: mean =15.0 sd=6.9 (Linton et al. best gamma distr fit)
meanOnsetToDeath <- 15.0
sdOnsetToDeath <- 6.9

### gamma distribution parameters for incubation period
shapeIncubation <- meanIncubation^2 / (sdIncubation^2)
scaleIncubation <- (sdIncubation^2) / meanIncubation

meanOnsetToCount <- c(
  "Confirmed cases" = meanOnsetToTest,
  "Deaths" = meanOnsetToDeath,
  "Hospitalized patients - admission" = meanOnsetToHosp,
  "Hospitalized patients" = meanOnsetToHosp,
  "Excess deaths" = meanOnsetToDeath)
sdOnsetToCount <- c(
  "Confirmed cases" = sdOnsetToTest,
  "Deaths" = sdOnsetToDeath,
  "Hospitalized patients - admission" = sdOnsetToHosp,
  "Hospitalized patients" = sdOnsetToHosp,
  "Excess deaths" = sdOnsetToDeath)

### parameters for gamma distribution between symptom onset and report
shapeOnsetToCount <- c(meanOnsetToCount^2 / (sdOnsetToCount^2), "Hospitalized patients - onset" = 0)
scaleOnsetToCount <- c((sdOnsetToCount^2) / meanOnsetToCount, "Hospitalized patients - onset" = 0)


###############

load(file = raw_data_path)
# 
# incidence_data <- rawData %>% 
#   filter( region == "Switzerland",
#           data_type == type,
#           variable == "incidence") %>%
#   select( date, value ) %>%
#   mutate( variable = "original")
# 
# ## start of what will be a function
# 
# min_chi_squared = 0.5
# maximum_iterations = 30
# initial_guess_delay <- 7

### Sample infection dates

##TESTING
swissData <- rawData %>% 
  filter( region == "Switzerland" )

# type <- "Confirmed cases"
# austriaData  <-  rawData %>% 
#   filter( region == "Austria", data_type == type, variable == "incidence" )
# 
# 
# discretized_distr <- discretize_waiting_time_distr(shape = c(shapeIncubation, shapeOnsetToCount[[type]]),
#                                                    scale = c(scaleIncubation, scaleOnsetToCount[[type]]))
# 
# ## TODO this is temporary
# first_guess_delay <- which.max(discretized_distr)[1] - 1
# 
# if( verbose ) {
#   cat("\tDelay on first guess: ", first_guess_delay, "\n")
# }
# 
# minimal_date <- min(data_subset$date) - first_guess_delay
# maximal_date <- max(data_subset$date)
# 
# smoothed_incidence_data <- data_subset %>%
#   mutate( value = getLOESSCases( dates = date,
#                                  count_data = value )) %>%
#   complete( date = seq.Date( minimal_date,
#                              maximal_date, 
#                              by = "days" )) %>%
#   mutate( value = if_else(is.na(value), 0, value )) %>%
#   mutate( variable = "smoothed")
# 
# 
# first_guess <- smoothed_incidence_data %>% 
#   mutate(date = date - first_guess_delay) %>% 
#   complete( date = seq.Date( minimal_date, maximal_date, by = "days" )) %>%
#   filter( date >=  minimal_date) %>%
#   mutate( value = if_else(is.na(value), 0, value ))
# 
# chi <- get_chi_squared(first_guess$value, smoothed_incidence_data$value, discretized_distr, max_delay = 6)
# 
# final_estimate <- iterate_RL( first_guess$value, 
#                               smoothed_incidence_data$value, 
#                               discretized_distr = discretized_distr, 
#                               threshold_chi_squared = min_chi_squared, 
#                               max_iterations = maximum_iterations, 
#                               max_delay = first_guess_delay,
#                               verbose = verbose )
# 
# ## right-truncate trailing zeroes induced by initial shift by 'first_guess_delay'
# deconvolved_dates <- first_guess %>%
#   filter( date <= maximal_date - first_guess_delay ) %>%
#   .$date
# deconvolved_infections <- final_estimate[ 1 : ( length(final_estimate) - first_guess_delay )]

  
## TODO fix bug in smoothing with Excess deaths
deconvolved_main_data <- get_all_infection_incidence(
  rawData,
  data_type = c("Confirmed cases",
                "Deaths"),
  #   data_type = c("Confirmed cases",
  #                 "Deaths",
  #                 "Excess deaths"),
  shapeIncubation = shapeIncubation,
  scaleIncubation = scaleIncubation,
  shapeOnsetToCount = shapeOnsetToCount,
  scaleOnsetToCount = scaleOnsetToCount,
  verbose = F)

deconvolved_FOPH_hosp_data <- get_all_infection_incidence(
  rawData,
  data_type = c("Hospitalized patients - admission", 
                "Hospitalized patients - onset"),
  shapeIncubation = shapeIncubation,
  scaleIncubation = scaleIncubation,
  shapeOnsetToCount = shapeOnsetToCount,
  scaleOnsetToCount = scaleOnsetToCount,
  min_chi_squared = 0.1,
  verbose = F)

## sum infections from Hospitalized patients - admission and Hospitalized patients - onset
deconvolved_FOPH_hosp_data <- deconvolved_FOPH_hosp_data %>%
  group_by(date, country, region, data_type, source, replicate, variable) %>%
  summarise(value=sum(value)) %>%
  arrange(country, region, source, data_type, variable, replicate, date) %>%
  ungroup()

deconvolved_infections <- bind_rows(deconvolved_main_data, deconvolved_FOPH_hosp_data) %>% ungroup()

saveRDS(deconvolved_infections, file = infect_data_path)

###############

cat(paste("###", Sys.time(), "- done 2_getInfectionIncidence.R", "\n"))
