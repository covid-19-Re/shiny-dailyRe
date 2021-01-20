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

# move files to live folder
cd "$parent_path"
mv ../data/temp/*.qs ../data/serialized/

# reload data by restarting R shiny process
touch ../restart.txt

# make plots
runRScript makeCHPlots.R
