#!/usr/bin/env bash

set -e # Make script fail if any command fails.
set -o pipefail # Make pipes fail if any command in pipe fails.


# Print version for reference.
./goblint --version
# This also checks if all linked dependencies are available (glibc (correct version), Apron, ...), crashes otherwise.
# Also check if goblint_runner exists and runs.
./goblint_runner.py --version


# Run smoke tests in subdirectory for convenience.
cd smoketests/
GOBLINT="../goblint_runner.py --portfolio-conf conf/svcomp26/seq.txt"
# This will also check if Goblint works when executed from different directory (finds Apron libs, conf, lib stubs), crashes otherwise.


# Check if architectures are supported (CIL Machdeps, C standard headers available for both) and return correct results.
# This is based on the cram test tests/regression/29-svcomp/36-svcomp-arch.t.
# There should be overflow on ILP32:
$GOBLINT --set ana.specification no-overflow.prp --set exp.architecture 32bit 36-svcomp-arch.c | grep "SV-COMP result: unknown"

# There shouldn't be an overflow on LP64:
$GOBLINT --set ana.specification no-overflow.prp --set exp.architecture 64bit 36-svcomp-arch.c | grep "SV-COMP result: true"


# Check if basic data race analysis returns correct results.
$GOBLINT --set ana.specification no-data-race.prp --set exp.architecture 32bit 04-mutex_01-simple_rc.c | grep "SV-COMP result: unknown"
$GOBLINT --set ana.specification no-data-race.prp --set exp.architecture 32bit 04-mutex_02-simple_nr.c | grep "SV-COMP result: true"


# Check if witness validation returns correct results.
GOBLINT_VALIDATOR="../goblint_runner.py --portfolio-conf conf/svcomp26/seq-validate.txt"

# From scratch verification actually succeeds for a variety of reasons (abortUnless analysis, wideningThresholds autotuner):
# This is not intentional.
$GOBLINT --set ana.specification unreach-call.prp --set exp.architecture 64bit mine2017-ex4.6.c | grep "SV-COMP result: true"

# Correct invariant should be confirmed:
$GOBLINT_VALIDATOR --set ana.specification unreach-call.prp --set exp.architecture 64bit mine2017-ex4.6.c --set witness.yaml.unassume mine2017-ex4.6-witness-correct.yml --set witness.yaml.validate mine2017-ex4.6-witness-correct.yml | grep "SV-COMP result: true"

# Imprecise invariant shouldn't be confirmed due to precision loss from unassuming it:
$GOBLINT_VALIDATOR --set ana.specification unreach-call.prp --set exp.architecture 64bit mine2017-ex4.6.c --set witness.yaml.unassume mine2017-ex4.6-witness-imprecise.yml --set witness.yaml.validate mine2017-ex4.6-witness-imprecise.yml | grep "SV-COMP result: unknown"

# Incorrect invariant shouldn't be confirmed:
$GOBLINT_VALIDATOR --set ana.specification unreach-call.prp --set exp.architecture 64bit mine2017-ex4.6.c --set witness.yaml.unassume mine2017-ex4.6-witness-incorrect.yml --set witness.yaml.validate mine2017-ex4.6-witness-incorrect.yml | grep "SV-COMP result: unknown"
