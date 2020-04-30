#!/bin/sh
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

git -C ../../../ch-hospital-data pull

RScript --vanilla --verbose getRawData.R > getRawData.Rout 2>&1
RScript --vanilla --verbose getInfectionIncidence.R > getInfectionIncidence.Rout 2>&1
RScript --vanilla --verbose doReEstimates.R > doReEstimates.Rout 2>&1

# crontab settings
# run everyday at 9am and 9pm
# 0 9,21 * * * /home/covid-19-re/covid-19-re-shiny-app/app/otherScripts/fetchAndAnalyzeData.sh
