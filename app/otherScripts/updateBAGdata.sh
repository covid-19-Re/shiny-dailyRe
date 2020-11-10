# for development on local macOS machine (on Linux just install the owncloud cli tools)
# symlink your polybox folder to ../data/BAG
# i.e. ln -s 'path/to/polybox/shared/BAG COVID19 Data' 'path/to/app/data/BAG'
owncloudcmd -n -s ~/BAGdata https://polybox.ethz.ch/remote.php/webdav/BAG%20COVID19%20Data

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)
cd "$parent_path"

latestDir=$(ls -td /home/covid-19-re/BAGdata/*/ | head -1)
if [ -f lastBAGdir.txt ]; then
  lastDir=$(<lastBAGdir.txt)
else
  lastDir="no latest dir found"
fi

if [ "$latestDir" != "$lastDir" ]; then
  ./runCountryEstimationCHE.sh
fi
echo "$latestDir">lastBAGdir.txt
