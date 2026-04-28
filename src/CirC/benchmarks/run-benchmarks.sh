#! /bin/bash

benchmarkdir=$1

if [ -z $benchmarkdir ]; then
  echo "Please specify benchmark dir"
  exit 1
fi

for bdir in $benchmarkdir/*; do
  if [ ! -d $bdir ]; then
    continue
  fi
  for benchmark in $benchmarkdir/$bdir/*.dl; do
    echo "Processing $benchmark..."
    base=$(basename $benchmark .dl)
    dir=$(dirname $benchmark)
    souffle $benchmark 2> /dev/null
    if diff infoLeak.csv $dir/$base.csv 2> /dev/null; then
      echo "OK"
    else
      echo "FAIL"
    fi
  done
done
