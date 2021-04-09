#!/bin/bash

# creduce ./scripts/privPrecCompare-creduce.sh ./pfscan_comb.c

set -e

gcc -c -Werror=implicit-function-declaration ./iowarrior.c

# OPTS="./pfscan_comb.c --enable custom_libc"
OPTS="./iowarrior.c --enable ana.sv-comp.functions"
# PRIVS=(global global-read global-history mine-W mine-lazy mine-global)
PRIVS=(global mine-W)
INTERESTING="mine-W: (Unknown int([0,64]))"
OUTDIR="privPrecCompare-creduce"
GOBLINTDIR="/home/simmo/dev/goblint/sv-comp/goblint"

mkdir -p $OUTDIR

for PRIV in "${PRIVS[@]}"; do
    echo $PRIV
    PRIVDUMP="$OUTDIR/$PRIV"
    rm -f $PRIVDUMP
    $GOBLINTDIR/goblint --sets exp.privatization $PRIV --sets exp.priv-prec-dump $PRIVDUMP $OPTS
done

PRIVDUMPS=("${PRIVS[*]/#/$OUTDIR/}") # why [*] here?
$GOBLINTDIR/_build/default/src/privPrecCompare.exe $PRIVDUMPS 2>&1 | tee "$OUTDIR/compare.txt"

grep -F "$INTERESTING" "$OUTDIR/compare.txt"