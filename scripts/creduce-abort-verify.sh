#!/usr/bin/env bash

set -e

gcc -c -Werror=implicit-function-declaration ./abort-verify.c

GOBLINTDIR="/mnt/goblint-svcomp/sv-comp/goblint"
OPTS="--conf $GOBLINTDIR/conf/svcomp.json --enable exp.solver.td3.abort --enable exp.solver.td3.abort-verify --sets ana.specification /mnt/goblint-svcomp/benchexec/sv-benchmarks/c/properties/unreach-call.prp --sets exp.architecture 64bit ./abort-verify.c"
INTERESTING="TD3 abort verify: should not abort"
OUTDIR="creduce-abort-verify"


mkdir -p $OUTDIR

LOG="$OUTDIR/out.log"
$GOBLINTDIR/goblint $OPTS -v --enable dbg.debug &> $LOG || true
grep -F "Function definition missing" $LOG && exit 1

grep -F "$INTERESTING" $LOG
