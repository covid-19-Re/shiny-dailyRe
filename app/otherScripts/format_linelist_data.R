if (interactive()) {
  library(here)
  library(tidyverse)
} else {
  suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
  })
}

outDir <- here::here("app", "data")

linelist_codes <- c("HKG")

for (code in linelist_codes) {
  source(here::here("app", "otherScripts", paste0("format_", code, "_data.R")))
}

delay_data <- c()
for (code in linelist_codes) {
  country_delay_data <- read_csv(here::here("app", "data", code, paste0(code, "_data_delays.csv")))
  delay_data <- c(delay_data, list(country_delay_data))
}

all_delays <- bind_rows(delay_data)

outDir <- here::here("app", "data")
write_csv(all_delays, path = file.path(outDir, "all_delays.csv"))
