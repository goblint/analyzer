#!/bin/sh

CONF="/home/arkocal/Workspace/goblint/fwd_rework/config.json"
GOBLINT="/home/arkocal/Workspace/goblint/fwd_rework/goblint"

# Run goblint with fwd solver
$GOBLINT --conf $CONF --solver fwd input.c > out.txt 2>&1

if [ $? -eq 3 ]; then
    grep Fixpoint out.txt >/dev/null 2>&1
else
    echo "Not error 3"
    exit 5
fi

