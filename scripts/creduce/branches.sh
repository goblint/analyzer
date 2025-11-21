#!/usr/bin/env bash

set -e

gcc -c -Werror=implicit-function-declaration ./bad.c

GOBLINTDIR="/home/simmo/dev/goblint/sv-comp/goblint"
OPTS="--conf $GOBLINTDIR/conf/svcomp25.json --set ana.specification $GOBLINTDIR/../sv-benchmarks/c/properties/unreach-call.prp bad.c --enable pre.enabled"
LOG="goblint.log"

$GOBLINTDIR/goblint $OPTS -v &> $LOG

grep -F "Both branches dead" $LOG
