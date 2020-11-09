### Utilities ###
filterRegions <- function(df, thresholdConfirmedCases = 500, thresholdHospitalizedPatients = 300, thresholdDeaths = 200) {
  regionsIncluded <- df %>%
    filter(data_type == "Confirmed cases") %>%
    dplyr::group_by(region) %>%
    dplyr::summarize(nCases = sum(value, na.rm = T), .groups = "drop") %>%
    filter(nCases >= thresholdConfirmedCases)
  excludedRegions <- setdiff(unique(df$region), regionsIncluded$region)
  dfout <- df %>%
    filter(region %in% regionsIncluded$region)
  
  regionsIncludedHospital <- dfout %>%
    filter(data_type == "Hospitalized patients") %>%
    dplyr::group_by(region) %>%
    dplyr::summarize(nCases = sum(value, na.rm = T), .groups = "drop") %>%
    filter(nCases >= thresholdHospitalizedPatients)
  
  excludedRegionsHospital <- setdiff(unique(dfout$region), regionsIncludedHospital$region)
  dfout <- dfout %>%
    filter(data_type != "Hospitalized patients" | region %in% regionsIncludedHospital$region)
  
  regionsIncludedDeaths <- dfout %>%
    filter(data_type == "Deaths") %>%
    dplyr::group_by(region) %>%
    dplyr::summarize(nCases = sum(value, na.rm = T), .groups = "drop") %>%
    filter(nCases >= thresholdDeaths)
  
  excludedRegionsDeaths <- setdiff(unique(dfout$region), regionsIncludedDeaths$region)
  dfout <- dfout %>%
    filter(data_type != "Deaths" | region %in% regionsIncludedDeaths$region)
  
  if(length(excludedRegions)  > 0) {
    cat(str_c(
      "\tDiscarded ", length(excludedRegions), " regions because threshold of ",
      thresholdConfirmedCases, " confirmed cases wasn't reached.\n",
      "\tDiscarded regions: ", str_c(excludedRegions, collapse = ", "), "\n"))
  }
  
  if(length(excludedRegionsHospital)  > 0) {
    cat(str_c(
      "\tDiscarded ", length(excludedRegionsHospital), " regions because cumulative threshold of ",
      thresholdHospitalizedPatients, " hospital admissions wasn't reached.\n",
      "\tDiscarded regions: ", str_c(excludedRegionsHospital, collapse = ", "), "\n"))
  }
  
  if(length(excludedRegionsDeaths)  > 0) {
    cat(str_c(
      "\tDiscarded ", length(excludedRegionsDeaths), " regions because cumulative threshold of ",
      thresholdDeaths, " deaths wasn't reached.\n",
      "\tDiscarded regions: ", str_c(excludedRegionsDeaths, collapse = ", "), "\n"))
  }

  
  return(dfout)
}

# smooth time series with LOESS method
getLOESSCases <- function(dates, count_data, days_incl = 21, degree = 1, truncation = 0) {
  
  if (truncation != 0) {
    dates <- dates[1:(length(dates) - truncation)]
    count_data <- count_data[1:(length(count_data) - truncation)]
  }
  
  n_points <- length(unique(dates))
  sel_span <- days_incl / n_points
  
  n_pad <- round(length(count_data) * sel_span * 0.5)
  
  c_data <- data.frame(value = c(rep(0, n_pad), count_data),
                       date_num = c(seq(as.numeric(dates[1]) - n_pad, as.numeric(dates[1]) - 1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = sel_span, degree = degree)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] <- 0
  raw_smoothed_counts <- smoothed[(n_pad + 1):length(smoothed)]
  normalized_smoothed_counts <-
    raw_smoothed_counts * sum(count_data, na.rm = T) / sum(raw_smoothed_counts, na.rm = T)
  
  if (truncation != 0) {
    normalized_smoothed_counts <- append(normalized_smoothed_counts, rep(NA, truncation))
  }
  return(normalized_smoothed_counts)
}

#### Build empirical CDF from draws summing samples from two gamma distributions
make_ecdf_from_gammas <- function(shape, scale, numberOfSamples = 1E6) {
  draws <-
    rgamma(numberOfSamples, shape = shape[1], scale = scale[1]) +
    rgamma(numberOfSamples, shape = shape[2], scale = scale[2])
  return(Vectorize(ecdf(draws)))
}

make_ecdf_from_empirical_data_and_gamma <- function(gamma_draws,
                                                    empirical_distr_onset_to_count){
  
  multiplier <- 100
  
  while(length(gamma_draws) < (length(empirical_distr_onset_to_count)*multiplier) & multiplier > 1) {
    multiplier <- floor(multiplier * 0.8)
  }
  
  if(multiplier == 1) {
    final_length <- min(length(gamma_draws), length(empirical_distr_onset_to_count))
    
    draws <- sample(gamma_draws, final_length, replace = F) + sample(empirical_distr_onset_to_count, final_length, replace = F)
  } else {
    draws <- gamma_draws[1:(length(empirical_distr_onset_to_count)*multiplier)] + rep(empirical_distr_onset_to_count, times=multiplier)
  }
  
  return(ecdf(draws))
}

make_ecdf_from_empirical_data <- function(empirical_distr_onset_to_count){
  
  multiplier <- 100
  
  while(length(gamma_draws) < (length(empirical_distr_onset_to_count)*multiplier) & multiplier > 1) {
    multiplier <- floor(multiplier * 0.8)
  }
  
  if(multiplier == 1) {
    final_length <- min(length(gamma_draws), length(empirical_distr_onset_to_count))
    
    draws <- sample(gamma_draws, final_length, replace = F) + sample(empirical_distr_onset_to_count, final_length, replace = F)
  } else {
    draws <- gamma_draws[1:(length(empirical_distr_onset_to_count)*multiplier)] + rep(empirical_distr_onset_to_count, times=multiplier)
  }
  
  return(ecdf(draws))
}

get_dates_to_average_over <- function(i, dates, weeks_averaged) {
  if (i < (weeks_averaged * 7)) {
    return(dates[seq_len(weeks_averaged * 7)])
  } else {
    return(dates[(i - (weeks_averaged * 7)):i])
  }
}


get_vector_constant_waiting_time_distr <- function(shape_incubation,
                                                   scale_incubation,
                                                   shape_onset_to_report,
                                                   scale_onset_to_report,
                                                   length_out = 200,
                                                   n_random_samples = 1E6) {
  
  F_h <- make_ecdf_from_gammas(shape = c(shape_incubation, shape_onset_to_report), scale = c(scale_incubation, scale_onset_to_report))
  
  f <- Vectorize(function(x){
    if(x < 0) {
      return(0)
    } else if(x < 0.5) {
      return(F_h(0.5))
    } else {
      return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
    }
  })
  
  x <- 0:(length_out - 1)
  
  return(f(x))
}

get_matrix_constant_waiting_time_distr <- function(waiting_time_distr,
                                                   all_dates) {
  N <- length(all_dates)
  
  if(length(all_dates) >= length(waiting_time_distr)) {
    waiting_time_distr <- c(waiting_time_distr, rep(0, times = N - length(waiting_time_distr)))
  }
  
  delay_distribution_matrix <- matrix(0, nrow = N, ncol = N)
  for(i in 1:N) {
    delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), waiting_time_distr[1:(N - i + 1)])
  }
  
  return(delay_distribution_matrix)
}

get_matrix_empirical_waiting_time_distr <- function(onset_to_report_empirical_delays,
                                                    all_dates,
                                                    min_number_cases = 300,
                                                    upper_quantile_threshold = 0.99){
  
  N <- length(all_dates)
  
  onset_to_report_empirical_delays <- onset_to_report_empirical_delays %>%
    filter(onset_date %in% all_dates)
  
  delay_counts <- onset_to_report_empirical_delays %>%
    dplyr::select(delay) %>% 
    group_by(delay) %>% 
    summarise(counts = n(), .groups = "drop")
  
  threshold_right_truncation <- delay_counts %>%  
    mutate(cumul_freq = cumsum(counts)/sum(counts)) %>% 
    filter(cumul_freq > upper_quantile_threshold) %>%
    head(n=1) %>% 
    pull(delay) 
  
  
  min_number_cases <- min(min_number_cases, sum(delay_counts$counts))
  
  delay_distribution_matrix <- matrix(0, nrow = N, ncol = N)
  
  i <- 1
  # populate delay_distribution_matrix by column
  for(i in 1:N) {
    
    if(i > (N - threshold_right_truncation) & N > threshold_right_truncation ) {
      
      delay_distribution_matrix[, i ] <-  c(0, delay_distribution_matrix[1:(N-1), i - 1 ])
      next
    }
    
    weeks_averaged <- 0
    repeat{
      weeks_averaged <- weeks_averaged + 1
      recent_counts_distribution <- onset_to_report_empirical_delays %>%
        dplyr::filter( onset_date %in% get_dates_to_average_over(i, all_dates, weeks_averaged))
      
      if(nrow( recent_counts_distribution ) >= min_number_cases) {
        break
      }
    }
    
    recent_delay_counts <-  recent_counts_distribution %>%
      dplyr::select(delay) %>% 
      group_by(delay) %>% 
      summarise(counts = n(), .groups = "drop") %>% 
      complete(delay  = seq(min(delay), max(delay)),
               fill = list(counts = 0)) 
    
    recent_delays <- recent_counts_distribution %>% pull(delay)
    
    gamma_fit <- try(fitdist(recent_delays + 1, distr = "gamma"))
    if ("try-error" %in% class(gamma_fit)) {
      cat("    mle failed to estimate the parameters. Trying method = \"mme\"\n")
      gamma_fit <- fitdist(recent_delays + 1, distr = "gamma", method = "mme")
    }
    
    shape_fit <- gamma_fit$estimate["shape"]
    rate_fit <- gamma_fit$estimate["rate"]
    
    
    last_index <- N - i + 1
    x <- (1:last_index) + 0.5
    x <- c(0, x)
    
    cdf_values <- pgamma(x, shape = shape_fit, rate = rate_fit)
    freq <- diff(cdf_values)
    
    if(length(freq) >= last_index) {
      delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), freq[1:last_index])
    } else {
      delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), freq[1:length(freq)], rep(0, times = last_index - length(freq)))
    }
  }
  
  return( delay_distribution_matrix )
}



get_bootstrap_replicate <- function(original_time_series) {
  replicate <- original_time_series %>%
    dplyr::slice_sample(n = sum(original_time_series$value, na.rm = T),
                        weight_by = replace_na(value, 0),
                        replace = T) %>%
    dplyr::group_by(country, region, source, data_type, date_type, local_infection, date) %>%
    dplyr::mutate(value = n()) %>%
    distinct(date, .keep_all = T) %>%
    ungroup() %>%
    dplyr::group_by(country, region, source, data_type, local_infection, date_type) %>%
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(value = 0)) %>%
    arrange(date)
  
  return(replicate)
}

iterate_RL <- function(
  initial_estimate,
  original_incidence,
  delay_distribution_matrix,
  threshold_chi_squared = 1,
  max_iterations = 100,
  max_delay,
  verbose = FALSE) {
  
  current_estimate <- initial_estimate
  N <- length(current_estimate)
  N0 <- N - max_delay
  chi_squared <- Inf
  count <- 1
  
  delay_distribution_matrix <- delay_distribution_matrix[1:length(current_estimate), 1:length(current_estimate)]
  truncated_delay_distribution_matrix <- delay_distribution_matrix[(1 + max_delay):NROW(delay_distribution_matrix),, drop = F]
  
  Q_vector <- apply(truncated_delay_distribution_matrix, MARGIN = 2, sum)
  
  while(chi_squared > threshold_chi_squared & count <= max_iterations) {
    
    if (verbose) {
      cat("\t\tStep: ", count, " - Chi squared: ", chi_squared, "\n")
    }
    
    E <- as.vector(delay_distribution_matrix %*% current_estimate)
    B <- replace_na(original_incidence/E, 0)
    
    current_estimate <- current_estimate / Q_vector *  as.vector(crossprod(B, delay_distribution_matrix))
    current_estimate <- replace_na(current_estimate, 0)
    
    chi_squared <- 1/N0 * sum((E[(max_delay + 1): length(E)] - original_incidence[(max_delay + 1) : length(original_incidence)])^2/E[(max_delay + 1): length(E)], na.rm = T)
    count <- count + 1
  }
  
  return(current_estimate)
}

# incidence_data is a tibble with 'date' and 'value' columns
do_deconvolution <- function(
  incidence_data,
  days_further_in_the_past = 30,
  verbose = FALSE,
  delay_distribution_matrix,
  initial_delta,
  max_iterations = 100
) {
  
  # use mode of 'constant_delay_distribution'. -1 because indices are offset by one as the delay can be 0.
  
  first_guess_delay <- initial_delta
  
  if (verbose) {
    cat("\tDelay on first guess: ", first_guess_delay, "\n")
  }
  
  first_recorded_incidence <-  with(filter(incidence_data, cumsum(value) > 0), value[which.min(date)])
  last_recorded_incidence <- with(incidence_data, value[which.max(date)])
  
  minimal_date <- min(incidence_data$date) - days_further_in_the_past
  maximal_date <- max(incidence_data$date)
  
  first_guess <- incidence_data %>%
    mutate(date = date - first_guess_delay) %>%
    complete(date = seq.Date(minimal_date, min(date), by = "days"),
             fill = list(value = first_recorded_incidence)) %>% # left-pad with first recorded value
    complete(date = seq.Date(max(date), maximal_date, by = "days"),
             fill = list(value = last_recorded_incidence)) %>% # right-pad with last recorded value
    arrange(date) %>% 
    filter(date >=  minimal_date)
  
  original_incidence <- incidence_data %>% 
    complete(date = seq.Date(minimal_date, maximal_date, by = "days"),
             fill = list(value = 0)) %>% 
    pull(value)
  
  final_estimate <- iterate_RL(
    first_guess$value,
    original_incidence,
    delay_distribution_matrix = delay_distribution_matrix,
    max_delay = days_further_in_the_past,
    max_iterations = max_iterations,
    verbose = verbose)
  
  deconvolved_dates <- first_guess %>% pull(date)
  
  result <- tibble(date = deconvolved_dates, value = final_estimate)
  
  result <- result %>%
    filter(date <= maximal_date - first_guess_delay)
  
  return(result)
}

get_infection_incidence_by_deconvolution <- function(
  data_subset,
  constant_delay_distribution,
  constant_delay_distribution_incubation = c(),
  is_onset_data = F,
  is_local_cases = T,
  smooth_incidence = T,
  empirical_delays  = tibble(),
  n_bootstrap = 5,
  days_further_in_the_past = 30,
  days_further_in_the_past_incubation = 5,
  max_iterations = 100,
  verbose = FALSE) {
  
  #TODO make the days_further_in_the_past type specific
  
  if(nrow(data_subset) == 0) {
    return(tibble())
  }
  
  data_type_subset <- unique(data_subset$data_type)[1]
  
  # exclude leading zeroes
  data_subset <- data_subset %>%
    arrange(date) %>%
    filter(cumsum(value) > 0)
  
  if(nrow(data_subset) == 0) {
    return(tibble())
  }
  
  minimal_date <- min(data_subset$date) - days_further_in_the_past
  maximal_date <- max(data_subset$date)
  all_dates <- seq(minimal_date, maximal_date, by = "days")
  
  is_empirical = (nrow(empirical_delays) > 0)
  
  if( is_onset_data ) {
    delay_distribution_matrix_incubation <- get_matrix_constant_waiting_time_distr(
      constant_delay_distribution_incubation,
      all_dates)
    
    initial_delta_incubation <- min(which(cumsum(constant_delay_distribution_incubation) > 0.5)) - 1 # take median value (-1 because index 1 corresponds to zero days)
    
    
    # account for additional right-truncation of onset data (needs to be reported first)
    if(is_empirical) {
      delay_distribution_matrix_onset_to_report <- get_matrix_empirical_waiting_time_distr(
        empirical_delays,
        seq.Date(min(data_subset$date), max(data_subset$date), by = "days"))
    } else {
      delay_distribution_matrix_onset_to_report <- get_matrix_constant_waiting_time_distr(
        constant_delay_distribution,
        seq.Date(min(data_subset$date), max(data_subset$date), by = "days"))
    }
    
    data_subset <- data_subset %>%
      complete(date = seq.Date(min(date), max(date), by = "days"), fill = list(value = 0))
    
    Q_vector_onset_to_report <- apply(delay_distribution_matrix_onset_to_report, MARGIN = 2, sum)
    
    if(unique(data_subset$region)[1] == "ESP") { # hack to work around spanish data between symptom onset dates only
      right_truncation <- 3
      # need to offset the Q vector by how many days were truncated off originally
      Q_vector_onset_to_report <- c(rep(1, right_truncation), Q_vector_onset_to_report[1:(length(Q_vector_onset_to_report) - right_truncation)] )
    }
    
    data_subset <- data_subset %>%
      mutate(value = value / Q_vector_onset_to_report) %>% 
      mutate(value = if_else(value == Inf, 0, value))
    
  } else {
    if(is_empirical) {
      delay_distribution_matrix_onset_to_report <- get_matrix_empirical_waiting_time_distr(
        empirical_delays,
        all_dates[(days_further_in_the_past_incubation + 1):length(all_dates)])
      
      delay_distribution_matrix_incubation <- get_matrix_constant_waiting_time_distr(
        constant_delay_distribution_incubation,
        all_dates)
      
      initial_delta_incubation <- min(which(cumsum(constant_delay_distribution_incubation) > 0.5)) - 1 # take median value (-1 because index 1 corresponds to zero days)
      initial_delta_report <-  median(empirical_delays$delay, na.rm = T)
    } else {
      delay_distribution_matrix <- get_matrix_constant_waiting_time_distr(
        constant_delay_distribution,
        all_dates)
      
      initial_delta <- min(which(cumsum(constant_delay_distribution) > 0.5)) - 1 # take median value (-1 because index 1 corresponds to zero days)
    }
  }
  
  
  
  results <- list(tibble())
  
  for (bootstrap_replicate_i in 0:n_bootstrap) {
    
    if (verbose == T) {
      cat("    Bootstrap replicate: ", bootstrap_replicate_i, "\n")
    }
    
    if (bootstrap_replicate_i == 0) {
      time_series <- data_subset
    } else {
      time_series <- get_bootstrap_replicate(data_subset)
    }
    
    if (smooth_incidence == T) {
      smoothed_incidence_data <- time_series %>%
        complete(date = seq.Date(min(date), max(date), by = "days"), fill = list(value = 0)) %>% 
        mutate(value = getLOESSCases(dates = date, count_data = value))
      
      raw_total_incidence <- sum(time_series$value, na.rm = TRUE)
      smoothed_total_incidence <- sum(smoothed_incidence_data$value, na.rm = T)
      
      if (smoothed_total_incidence > 0) {
        smoothed_incidence_data <- smoothed_incidence_data %>%
          mutate(value = value * raw_total_incidence / smoothed_total_incidence)
      }
      
    } else {
      smoothed_incidence_data <- time_series  %>%
        complete(date = seq.Date(min(date), max(date), by = "days"), fill = list(value = 0))
    }
    
    
    if (is_onset_data) {
      deconvolved_infections <-  do_deconvolution(smoothed_incidence_data,
                                                  delay_distribution_matrix = delay_distribution_matrix_incubation,
                                                  days_further_in_the_past = days_further_in_the_past,
                                                  initial_delta = initial_delta_incubation,
                                                  max_iterations = max_iterations,
                                                  verbose = verbose)
    } else {
      if(is_empirical) {
        # perform the deconvolution in two steps
        deconvolved_symptom_onsets <- do_deconvolution(smoothed_incidence_data,
                                                       delay_distribution_matrix = delay_distribution_matrix_onset_to_report,
                                                       days_further_in_the_past = days_further_in_the_past - days_further_in_the_past_incubation,
                                                       initial_delta = initial_delta_report,
                                                       max_iterations = max_iterations,
                                                       verbose = verbose)
        
        deconvolved_infections <- do_deconvolution(deconvolved_symptom_onsets,
                                                   delay_distribution_matrix = delay_distribution_matrix_incubation,
                                                   days_further_in_the_past = days_further_in_the_past_incubation,
                                                   initial_delta = initial_delta_incubation,
                                                   max_iterations = max_iterations,
                                                   verbose = verbose)
      } else {
        deconvolved_infections <-  do_deconvolution(smoothed_incidence_data,
                                                    delay_distribution_matrix = delay_distribution_matrix,
                                                    days_further_in_the_past = days_further_in_the_past,
                                                    initial_delta = initial_delta,
                                                    max_iterations = max_iterations,
                                                    verbose = verbose)
      }
    }
    
    
    deconvolved_infections <- deconvolved_infections %>% slice((days_further_in_the_past -5 + 1):n())
    
    data_type_name <- paste0("infection_", data_type_subset)
    
    ## dataframe containing results
    deconvolved_infections <- tibble(
      date = deconvolved_infections$date,
      region = unique(time_series$region)[1],
      country = unique(time_series$country)[1],
      source = unique(time_series$source)[1],
      local_infection = is_local_cases,
      data_type = data_type_name,
      replicate = bootstrap_replicate_i,
      value = deconvolved_infections$value
    )
    
    results <- c(results, list(deconvolved_infections))
  }
  
  return(bind_rows(results))
}

### One gamma-distributed waiting time is the incubation period
### The second one is the period between symptom onset and report (of positive test, death, hospitalization...)
get_all_infection_incidence <- function(data,
                                        constant_delay_distributions,
                                        onset_to_count_empirical_delays = tibble(),
                                        data_types,
                                        n_bootstrap = 100,
                                        verbose = FALSE) {
  
  results <- list(tibble())
  
  is_delays_data_available <- (nrow(onset_to_count_empirical_delays) > 0)
  
  for (count_type_i in data_types) {
    
    cat("Deconvolve infections for data type:", count_type_i, "\n")
    
    smooth <- (count_type_i != "Excess deaths")
    
    for (source_i in unique(data$source)) {
      
      cat("  Data source:", source_i, "\n")
      
      # nCores <- max(1, parallel::detectCores() - 1)
      # cat("   calculating on", nCores, "cores...\n")
      # cl <- parallel::makeCluster(nCores, type = "FORK", outfile = "")
      
      
      for(local_infection_i in c(TRUE, FALSE)) {
        
        results_list <- lapply(# parallel::parLapply(cl,
          unique(data$region),
          function(x) {
            cat("    Region:", x, "\n")
            subset_data <- data %>%
              filter(region == x,
                     source == source_i,
                     data_type == count_type_i,
                     local_infection == local_infection_i) %>%
              arrange(date)
            
            country_i <- subset_data %>% distinct(country)
            
            if (nrow(subset_data) == 0) {
              return(tibble())
            }
            
            if (is_delays_data_available) {
              empirical_delays <- onset_to_count_empirical_delays %>%
                filter(
                  country %in% country_i,
                  data_type == count_type_i)
              
            } else {
              empirical_delays <- tibble()
            }
            
            subset_data_report <- subset_data %>% filter(date_type == "report")
            last_date_report <- max(subset_data$date)
            
            deconvolved_reports <- get_infection_incidence_by_deconvolution(
              subset_data_report,
              constant_delay_distribution = constant_delay_distributions[[count_type_i]],
              constant_delay_distribution_incubation = constant_delay_distributions[["Symptoms"]],
              is_onset_data = F,
              is_local_cases = local_infection_i,
              smooth_incidence = smooth,
              empirical_delays = empirical_delays,
              n_bootstrap = n_bootstrap,
              verbose = verbose)
            
            if(nrow(deconvolved_reports) > 0) {
              last_date_report <- max(deconvolved_reports$date)
            }
            
            subset_data_onset <- subset_data %>% filter(date_type == "onset")
            
            deconvolved_onset <- get_infection_incidence_by_deconvolution(
              subset_data_onset,
              constant_delay_distribution = constant_delay_distributions[[paste0('Onset to ', count_type_i)]],
              constant_delay_distribution_incubation = constant_delay_distributions[["Symptoms"]],
              is_onset_data = T,
              is_local_cases = local_infection_i,
              smooth_incidence = smooth,
              empirical_delays = empirical_delays,
              n_bootstrap = n_bootstrap,
              verbose = verbose)
            
            if(nrow(deconvolved_onset) > 0) {
              deconvolved_onset <- deconvolved_onset %>% filter(date <= last_date_report) # if two types of data (onset and report) are there, filter out last onset deconvolved data (bc incomplete)
            }
            
            if((nrow(deconvolved_onset) + nrow(deconvolved_reports)) > 0) {
              combined_deconvolved <- bind_rows(deconvolved_reports, deconvolved_onset) %>% 
                dplyr::group_by(date, region, country, replicate, source, data_type, local_infection) %>% 
                dplyr::summarise(value = sum(value), .groups = "keep") %>%
                arrange(country, region, source, data_type, replicate, local_infection, date) %>%
                ungroup()
              
              return(combined_deconvolved)
            } else {
              return(tibble())
            }
          })
        # }
        #parallel::stopCluster(cl)
        results <- c(results, results_list)
      }
    }
  }
  
  combined_result <- bind_rows(results)
  
  combined_result <- combined_result %>%  
    group_by(region, country, replicate, source, data_type) %>% 
    complete(date = seq(min(date), max(date), by = "days"), 
             local_infection,
             region, 
             country, 
             replicate, 
             source, 
             data_type, 
             fill = list(value = 0)) %>% 
    ungroup() %>% 
    arrange(country, region, source, data_type, replicate, local_infection, date)
  
  
  return(combined_result)
}


