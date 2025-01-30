#!/usr/bin/env bash

# ./scripts/privPrecCompare.sh ../goblint-bench/pthread/pfscan_comb.c --enable custom_libc

PRIVS=(protection protection-read write mine-W lock write+lock)
OUTDIR="privPrecCompareRes"

mkdir -p $OUTDIR

for PRIV in "${PRIVS[@]}"; do
    echo $PRIV
    PRIVDUMP="$OUTDIR/$PRIV"
    ./goblint --set exp.privatization $PRIV --set exp.priv-prec-dump $PRIVDUMP "$@"
done

PRIVDUMPS=("${PRIVS[*]/#/$OUTDIR/}") # why [*] here?
./privPrecCompare $PRIVDUMPS 2>&1 | tee "$OUTDIR/compare.txt"
