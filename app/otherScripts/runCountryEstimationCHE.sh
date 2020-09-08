#!/bin/sh
parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

runRScript () {
  echo "running" $1 $2 "..."
  Rscript --vanilla --verbose $1 $2 >>messagesCHE.Rout 2>>errorsCHE.Rout
  retVal=$?
  if [ $retVal -ne 0 ]; then
    echo "Script didn't run successfully (Error" $retVal ")"
  else
    echo "       " $1 $2 "done"
  fi
}

cd "$parent_path"

echo "updating covid19-additionalData ..."
cd "../../../covid19-additionalData"
git reset --hard HEAD
git pull

cd "$parent_path"
rm *.Rout

echo "running R script to extract BAG data & calculate delays ..."
runRScript format_BAG_data.R

runRScript ReCountry.R "CHE"
runRScript makeCHPlots.R

runRScript sumData.R

# TODO uncomment
# echo "updating covid19-Data ..."
# cd "../../../dailyRe-Data"
# git add .
# git commit -m "update data"
# git push
