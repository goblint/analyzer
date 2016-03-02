#!/bin/bash
if [ $# -lt 2 ]; then
    echo "Usage: $0 group-nr test-nr [extra options]"
    exit 1
fi
file="tests/regression/$1*/$2*.c"
if [ ! -e $file ]; then
  echo "No file found!"
  exit 1
fi
params="`grep -oP "PARAM: \K.*" $file`"
cmd="./goblint --enable colors --enable dbg.debug --enable dbg.verbose --enable dbg.showtemps $params ${@:3} $file"
echo "$cmd"
eval $cmd
