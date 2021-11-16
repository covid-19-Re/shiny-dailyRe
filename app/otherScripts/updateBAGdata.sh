# for development on local macOS machine (on Linux just install the owncloud cli tools)
# symlink your polybox folder to ../data/BAG
# i.e. ln -s 'path/to/polybox/shared/BAG COVID19 Data' 'path/to/app/data/BAG'
owncloudcmd -n -s ~/BAGdata https://polybox.ethz.ch/remote.php/webdav/BAG%20COVID19%20Data
owncloudcmd -n -s ~/ICUdata https://polybox.ethz.ch/remote.php/webdav/Shared/outputTaskforce

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)
cd "$parent_path"

latestFile=$(ls -t /home/covid-19-re/BAGdata/*/*FOPH_COVID19_data_extract.csv | head -1)
if [ -f /home/covid-19-re/lastBAGFile.txt ]; then
  lastFile=$(</home/covid-19-re/lastBAGFile.txt)
else
  lastFile="no latest dir found"
fi

if [ "$latestFile" != "$lastFile" ]; then
  # check if the data is from today
  latestFileDate=${latestFile:37:10}
  currentDate=`date "+%Y-%m-%d"`
  if [ "$currentdate" == "$latestFileDate" ]; then
      ./runCountryEstimationCHE.sh
      echo "$latestFile">/home/covid-19-re/lastBAGFile.txt
  fi
fi

