#!/bin/bash

# ./scripts/privPrecCompare.sh ../goblint-bench/pthread/pfscan_comb.c --enable custom_libc

PRIVS=(global global-read global-history mine-W mine-lazy mine-global)
OUTDIR="privPrecCompare"

mkdir -p $OUTDIR

for PRIV in "${PRIVS[@]}"; do
    echo $PRIV
    PRIVDUMP="$OUTDIR/$PRIV"
    ./goblint --sets exp.privatization $PRIV --sets exp.priv-prec-dump $PRIVDUMP "$@"
done

PRIVDUMPS=("${PRIVS[*]/#/$OUTDIR/}") # why [*] here?
./_build/default/src/privPrecCompare.exe $PRIVDUMPS 2>&1 | tee "$OUTDIR/compare.txt"
