### Waiting time distributions ##
## hardcoded for now, but to be taken outside of script
# incubation: mean = 5.3, sd = 3.2 (Linton et al., best gamma distr fit)
mean_incubation <- 5.3
sd_incubation <- 3.2

# onset to test:
mean_onset_to_test <- 5.5 #Bi et al.
sd_onset_to_test <- 3.8

# onset to hospitalization report: Pellis et al. 2020
mean_onset_to_hosp <- 5.14
sd_onset_to_hosp <- 4.2

# onset to death: mean =15.0 sd=6.9 (Linton et al. best gamma distr fit)
mean_onset_to_death <- 15.0
sd_onset_to_death <- 6.9

### gamma distribution parameters for incubation period
shape_incubation <- mean_incubation^2 / (sd_incubation^2)
scale_incubation <- (sd_incubation^2) / mean_incubation

mean_onset_to_count <- c(
  "Confirmed cases" = mean_onset_to_test,
  "Confirmed cases / tests" = mean_onset_to_test,
  "Deaths" = mean_onset_to_death,
  "Hospitalized patients" = mean_onset_to_hosp,
  "Excess deaths" = mean_onset_to_death)
sd_onset_to_count <- c(
  "Confirmed cases" = sd_onset_to_test,
  "Confirmed cases / tests" = sd_onset_to_test,
  "Deaths" = sd_onset_to_death,
  "Hospitalized patients" = sd_onset_to_hosp,
  "Excess deaths" = sd_onset_to_death)

### parameters for gamma distribution between symptom onset and report
shape_onset_to_count <- c(mean_onset_to_count^2 / (sd_onset_to_count^2), "Symptoms" = 0)
scale_onset_to_count <- c((sd_onset_to_count^2) / mean_onset_to_count, "Symptoms" = 0)
