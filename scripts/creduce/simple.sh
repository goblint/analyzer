#!/bin/bash

# Run creduce like:
# creduce --timeout 900 ./scripts/creduce/incremental.sh /home/feniuk/University-Coding/bench/coreutils/cksum_comb.minimized.c


GOBLINTDIR="/home/feniuk/University-Coding/analyzer-pentagon/goblint"
INPUTFILE="/home/feniuk/University-Coding/bench/coreutils/cut_comb.minimized.c"
OUTPUTFILE="fe8f7af3a.out"
PARAMS="--set ana.activated[+] pentagon --enable allglobs --enable dbg.timing.enabled"

$GOBLINTDIR $INPUTFILE $PARAMS -v &> out.txt;
if [ $? -eq 2 ]; then
    gcc $INPUTFILE -o $OUTPUTFILE  && rm $OUTPUTFILE &> /dev/null &&
    grep "Fatal error: exception IntDomain0.ArithmeticOnIntegerBot(\"Error int op 8\")" "out.txt" >/dev/null 2>&1
else
    exit 5
fi
