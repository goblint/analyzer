#!/bin/bash
if [ $# -lt 2 ]; then
    echo "Usage: $0 group-nr test-nr [extra options]"
    exit 1
fi
file=(tests/regression/$1*/$2*.c)
if [ ! -e $file ]; then
  echo "No file found!"
  exit 1
fi
params="`grep -oP "PARAM: \K.*" $file`"
cmd="./goblint --enable dbg.debug --sets warnstyle \"legacy\" --enable colors --enable dbg.verbose --enable dbg.showtemps --enable dbg.regression --enable printstats --html $params ${@:3} $file"
echo "$cmd"
eval $cmd
echo "See result/index.xml"