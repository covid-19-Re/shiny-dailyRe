#!/bin/sh
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

cd "../../../ch-hospital-data"
git pull

cd "$parent_path"
Rscript --vanilla --verbose getRawData.R > getRawData.Rout 2>&1
Rscript --vanilla --verbose getInfectionIncidence.R > getInfectionIncidence.Rout 2>&1
Rscript --vanilla --verbose doReEstimates.R > doReEstimates.Rout 2>&1
# format data in seperate process to avoid C stack limit
Rscript --vanilla --verbose doReEstimatesFormat.R > doReEstimatesFormat.Rout 2>&1

# crontab settings
# run everyday at 9am and 9pm
# 0 0,8,16 * * * /home/covid-19-re/covid-19-re-shiny-app/app/otherScripts/fetchAndAnalyzeData.sh
