#!/usr/bin/env bash
#MacOS: needs brew install grep
if [ $# -lt 2 ]; then
    echo "Usage: $0 group-nr test-nr [extra options]"
    exit 1
fi
file=(tests/regression/$1*/$2*.c)
if [ ! -e $file ]; then
  echo "No file found!"
  exit 1
fi
grep="grep"
if [[ $OSTYPE == 'darwin'* ]]; then
  grep="ggrep"
fi
params="`$grep -oP "PARAM: \K.*" $file`"
cmd="./goblint --enable dbg.debug --enable dbg.regression --html $params ${@:3} $file" # -v
echo "$cmd"
eval $cmd
echo "See result/index.xml"
