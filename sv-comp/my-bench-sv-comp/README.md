# my-bench-sv-comp
This directory contains BenchExec benchmark and table definitions for a number of use cases and shell scripts for running them.

## goblint-all-fast
Run Goblint on a large number of reachability benchmarks with decreased timeout.

Files:
* `goblint-all-fast.sh`
* `goblint-all-fast.xml`
* `table-generator-all-fast.xml`


## goblint-data-race
Run Goblint on data-race benchmarks.

Files:
* `goblint-data-race.sh`
* `goblint-data-race.xml`
* `table-generator-data-race.xml`


## goblint-lint
Run Goblint and validate witnesses using witnesslinter.

Files:
* `goblint-lint.sh`
* `goblint-lint.xml`
* `table-generator-lint.xml`
* `witnesslint-validate.xml`


## goblint
Run Goblint and validate witnesses using:
* CPAChecker,
* Ultimate Automizer,
* witnesslinter.

Files:
* `cpa-validate-correctness.xml`
* `cpa-validate-violation.xml`
* `goblint.sh`
* `goblint.xml`
* `table-generator-witness.xml`
* `uautomizer-validate-correctness.xml`
* `uautomizer-validate-violation.xml`
* `witnesslint-validate2.xml`
