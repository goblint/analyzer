#!/bin/bash

# ./scripts/privPrecCompare.sh ../goblint-bench/pthread/pfscan_comb.c --enable custom_libc

PRIVS=(protection write mine-W lock write+lock)
OUTDIR="privPrecCompare"

mkdir -p $OUTDIR

for PRIV in "${PRIVS[@]}"; do
    echo $PRIV
    PRIVDUMP="$OUTDIR/$PRIV"
    ./goblint --sets exp.privatization $PRIV --sets exp.priv-prec-dump $PRIVDUMP "$@"
done

PRIVDUMPS=("${PRIVS[*]/#/$OUTDIR/}") # why [*] here?
./_build/default/src/privPrecCompare.exe $PRIVDUMPS 2>&1 | tee "$OUTDIR/compare.txt"
