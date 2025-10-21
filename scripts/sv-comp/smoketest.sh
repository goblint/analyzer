#!/usr/bin/env bash

set -e # Make script fail if any command fails.
set -o pipefail # Make pipes fail if any command in pipe fails.


# Print version for reference.
./goblint --version
# This also checks if all linked dependencies are available (glibc (correct version), Apron, ...), crashes otherwise.


# Run smoke tests in subdirectory for convenience.
cd smoketests/
GOBLINT="../goblint --conf conf/svcomp26.json"
# This will also check if Goblint works when executed from different directory (finds Apron libs, conf, lib stubs), crashes otherwise.


# From scratch verification actually succeeds for a variety of reasons (abortUnless analysis, wideningThresholds autotuner):
# This is not intentional.
$GOBLINT --set ana.specification unreach-call.prp --set exp.architecture 64bit mine2017-ex4.6.c | grep "SV-COMP result: true"

# Verification with Witch succeeds by giving a "false" verdict
$GOBLINT --set ana.specification unreach-call.prp --set exp.architecture 64bit implicitfloatconversion.c | grep "SV-COMP result: false"
