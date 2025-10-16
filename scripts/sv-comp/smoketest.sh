#!/usr/bin/env bash

set -e # Make script fail if any command fails.
set -o pipefail # Make pipes fail if any command in pipe fails.


# Print version for reference.
./goblint --version
# This also checks if all linked dependencies are available (glibc (correct version), Apron, ...), crashes otherwise.


# Run smoke tests in subdirectory for convenience.
cd smoketests/
GOBLINT="../goblint --conf conf/svcomp26.json"
# This also checks if Goblint works when executed from different directory (finds Apron libs, conf, lib stubs), crashes otherwise.


# Check if architectures are supported (CIL Machdeps, C standard headers available for both) and return correct results.
# This is based on the cram test tests/regression/29-svcomp/36-svcomp-arch.t.
# There should be overflow on ILP32:
$GOBLINT --set ana.specification no-overflow.prp --set exp.architecture 32bit 36-svcomp-arch.c | grep "SV-COMP result: unknown"

# There shouldn't be an overflow on LP64:
$GOBLINT --set ana.specification no-overflow.prp --set exp.architecture 64bit 36-svcomp-arch.c | grep "SV-COMP result: true"


# Check if basic data race analysis returns correct results.
$GOBLINT --set ana.specification no-data-race.prp --set exp.architecture 32bit 04-mutex_01-simple_rc.i | grep "SV-COMP result: unknown"
$GOBLINT --set ana.specification no-data-race.prp --set exp.architecture 32bit 04-mutex_02-simple_nr.i | grep "SV-COMP result: true"


# TODO: test validator
