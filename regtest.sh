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
cmd="./goblint --enable dbg.fail_on_different_ikind --enable dbg.debug --sets warnstyle \"legacy\" --enable colors --enable dbg.showtemps --enable dbg.regression --html $params ${@:3} $file" #  --enable dbg.verbose --enable printstats
cmd=`echo "$cmd" | sed "s:ana.osek.oil :ana.osek.oil $(dirname $file)/:"` # regression tests are run inside the test's directory which is why we either also need to cd there or instead prepend the path to the test directory for file parameters like these .oil files
echo "$cmd"
eval $cmd
echo "See result/index.xml"
