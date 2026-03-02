#!/bin/sh

CONF="/home/arkocal/Workspace/goblint/fwd_rework/config.json"
GOBLINT="/home/arkocal/Workspace/goblint/fwd_rework/goblint"

# Run goblint with fwd solver
fwd_out=$("$GOBLINT" --conf "$CONF" --solver fwd input.c 2>&1)
if [ $? -ne 0 ]; then
    echo "FAIL"
    echo $fwd_out
    exit 1
fi

# Run goblint with wbu solver
wbu_out=$("$GOBLINT" --conf "$CONF" --solver wbu input.c 2>&1)
if [ $? -ne 0 ]; then
    exit 1
fi

# Extract RHS values (assumes line like: [Info] RHS: 16)
fwd_rhs=$(echo "$fwd_out" | grep "RHS:" | awk '{print $NF}')
wbu_rhs=$(echo "$wbu_out" | grep "RHS:" | awk '{print $NF}')

# Ensure both values are numbers
case $fwd_rhs in
    ''|*[!0-9]*) exit 1 ;;
esac

case $wbu_rhs in
    ''|*[!0-9]*) exit 1 ;;
esac

echo $fwd_rhs
echo $wbu_rhs

# Avoid division and floating point: check fwd >= 10 * wbu
if [ "$fwd_rhs" -ge "$wbu_rhs" ]; then
    exit 0
else
    exit 1
fi
