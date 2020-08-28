#!/bin/sh
parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

countries=$(<"$parent_path/countries.txt")
echo "loop"
for i in ${countries[@]}; do
	echo "iso3"
  echo $i
done
