#!/bin/bash
./configure
make || exit 125
rm goblint.json

goblint tests/regression/01-cpa/01-expressions.c --result pretty | grep '__builtin_strncpy'
RETVAL=$(( ! $?))
exit $RETVAL
