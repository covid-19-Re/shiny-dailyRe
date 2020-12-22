#!/bin/sh
runRScript () {
  echo "running" $1 $2 "..."
  Rscript --verbose $1 $2 >>messagesCHE.Rout 2>>errorsCHE.Rout
  retVal=$?
  if [ $retVal -ne 0 ]; then
    echo "Script didn't run successfully (Error" $retVal ")"
  else
    echo "       " $1 $2 "done"
  fi
}

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

cd "$parent_path"

echo "updating covid19-Data ..."

# if on test server copy csv files
FILE=isTestServer.txt
if test -f "$FILE"; then
  cp /home/covid-19-re/test-dailyRe/app/data/countryData/csv/CHE-*.csv /home/covid-19-re/dailyRe-Data/
  cp /home/covid-19-re/test-dailyRe/app/data/countryData/csv/LIE-*.csv /home/covid-19-re/dailyRe-Data/
fi

# update the git
cd "../../../dailyRe-Data"
git add .
git commit -m "update data"
git push

# move files to live folder
cd "$parent_path"
mv ../data/temp/*.qs ../data/serialized/

# reload data by restarting R shiny process
touch ../restart.txt

# make plots
runRScript makeCHPlots.R

# update plots on eth cms
cd "../www/cantonPlots"
curl -nT "{$(echo *.png | tr ' ' ',')}" https://cms-author.ethz.ch/content/dam/ethz/special-interest/usys/ibz/theoreticalbiology/plots/

# send slack Notification
cd "$parent_path"
runRScript dataLiveNotification.R
