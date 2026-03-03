#!/bin/sh

CONF="/home/arkocal/Workspace/goblint/fwd_rework/config.json"
GOBLINT="/home/arkocal/Workspace/goblint/fwd_rework/goblint"

# Run goblint with fwd solver
"$GOBLINT" --conf "$CONF" --solver fwd input.c &> out.txt

if [ $? -eq 3 ]; then
    grep Fixpoint out.txt >/dev/null 2>&1
else
    exit 5
fi

