#!/usr/bin/env bash

set -e

HERE="/mnt/goblint-svcomp/vesal/analyzer"
FILE="aget_comb"

grep -F "case 2:" $FILE.c
gcc -c -Werror=implicit-function-declaration $FILE.c

INTERESTING="more precise using right"

$HERE/goblint --enable incremental.save $FILE.c &> before.log
#grep -F "Function definition missing" before.log && exit 1

sed "s/case 2:/case 2:\n    case 3:/" $FILE.c > $FILE.new.c
$HERE/goblint --enable incremental.load --set save_run incrementalrun $FILE.new.c &> after.incr.log
$HERE/goblint --set save_run originalrun $FILE.new.c &> after.scratch.log

$HERE/goblint --enable dbg.compare_runs.diff --compare_runs originalrun incrementalrun $FILE.new.c &> out.log
grep -F "$INTERESTING" out.log

