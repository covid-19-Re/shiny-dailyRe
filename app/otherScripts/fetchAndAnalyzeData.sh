#!/bin/sh
parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

checkScriptStatus () {
  retVal=$?
  if [ $retVal -ne 0 ]; then
    echo "Script didn't run successfully (Error" $retVal ")"
    exit 1
  fi
}

cd "$parent_path"

echo "updating ch-hospital-data ..."
cd "../../../ch-hospital-data"
git reset --hard HEAD
git pull
echo "updating covid19-additionalData ..."
cd "../covid19-additionalData"
git reset --hard HEAD
git pull

cd "$parent_path"
rm *.Rout
# make temp data directory and clean last temp files
mkdir -p ../data/temp
rm -f ../data/temp/*
echo "updating BAG Data (polybox sync)"
# for development on local macOS machine (on Linux just install the owncloud cli tools)
# symlink your polybox folder to ../data/BAG
# i.e. ln -s 'path/to/polybox/shared/BAG COVID19 Data' 'path/to/app/data/BAG'
owncloudcmd -n -s ../data/BAG \
  https://polybox.ethz.ch/remote.php/webdav/BAG%20COVID19%20Data
echo "running R script to extract BAG data ..."
Rscript --vanilla --verbose format_BAG_data.R >>messages.Rout 2>>errors.Rout
checkScriptStatus
echo "running R data analysis scripts ..."
Rscript --vanilla --verbose 1_getRawData.R >>messages.Rout 2>>errors.Rout
checkScriptStatus
Rscript --vanilla --verbose 2_getInfectionIncidence.R >>messages.Rout 2>>errors.Rout
checkScriptStatus
Rscript --vanilla --verbose 3_doReEstimates.R >>messages.Rout 2>>errors.Rout
checkScriptStatus
# summarize data in seperate process to avoid C stack limit
Rscript --vanilla --verbose 4_doReEstimatesSum.R >>messages.Rout 2>>errors.Rout
checkScriptStatus
Rscript --vanilla --verbose 5_makeReffPlotly.R >>messages.Rout 2>>errors.Rout
checkScriptStatus
# copy data
cp -f ../data/temp/* ../data/
# make app restart on next connection
touch ../restart.txt

# crontab settings
# run every 3h starting at 1am
# 0 1,4,7,10,13,16,19,22 * * * /home/covid-19-re/covid-19-re-shiny-app/app/otherScripts/fetchAndAnalyzeData.sh
