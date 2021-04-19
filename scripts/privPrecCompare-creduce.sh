#!/bin/bash

# creduce ./scripts/privPrecCompare-creduce.sh ./pfscan_comb.c

set -e

gcc -c -Werror=implicit-function-declaration ./tegra20.c

GOBLINTDIR="/home/simmo/dev/goblint/sv-comp/goblint"
# OPTS="./pfscan_comb.c --enable custom_libc"
OPTS="./tegra20.c --conf $GOBLINTDIR/conf/traces.json --enable ana.sv-comp.functions"
# PRIVS=(protection protection-read global-history mine-W mine-lazy mine-global)
PRIVS=(protection global-history)
INTERESTING="protection precise than global-history"
OUTDIR="privPrecCompare-creduce"


mkdir -p $OUTDIR

for PRIV in "${PRIVS[@]}"; do
    echo $PRIV
    PRIVDUMP="$OUTDIR/$PRIV"
    LOG="$OUTDIR/$PRIV.log"
    rm -f $PRIVDUMP
    $GOBLINTDIR/goblint --sets exp.privatization $PRIV --sets exp.priv-prec-dump $PRIVDUMP $OPTS -v --enable dbg.debug &> $LOG
    grep -F "Function definition missing" $LOG && exit 1
done

PRIVDUMPS=("${PRIVS[*]/#/$OUTDIR/}") # why [*] here?
$GOBLINTDIR/_build/default/src/privPrecCompare.exe $PRIVDUMPS 2>&1 | tee "$OUTDIR/compare.txt"

grep -F "$INTERESTING" "$OUTDIR/compare.txt"