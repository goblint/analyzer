#!/bin/bash
#git bisect start HEAD {last known good revision}
#git bisect run ./scripts/bisect.sh
./make.sh opt || exit 125
./scripts/update_suite.rb

#goblint tests/regression/01-cpa/01-expressions.c --result pretty | grep '__builtin_strncpy'
#RETVAL=$(( ! $?))
#exit $RETVAL
