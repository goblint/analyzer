# Testing

## Regression tests
Regression tests are small programs that can be used to quickly verify that existing functionality hasn't been broken.
They can be found in `./tests/regression/`.
Options that should be passed to Goblint when executing a regression test are specified in the test file using a single line comment starting with `//PARAM:`.

### Running
Regression tests can be run with various granularity:

* Run all tests with: `./scripts/update_suite.rb`.
* Run a group of tests with: `./scripts/update_suite.rb group sanity`.

    Unfortunately this also runs skipped tests...

* Run a single test with: `./scripts/update_suite.rb assert`.
* Run a single test with full output: `./regtest.sh 00 01`.

    Additional options are also passed to Goblint.

To pass additional options to Goblint with `update_suite.rb`, use the `gobopt` environment variable, e.g.:
```
gobopt='--set ana.base.privatization write+lock' ./scripts/update_suite.rb
```

### Writing
* Add parameters to a regression test in the first line: `// PARAM: --set dbg.debug true`
* Annotate lines inside the regression test with comments: `arr[9] = 10; // WARN`

## Incremental tests
The incremental tests are regression tests that are first run with the option `incremental.save` and then again
incrementally (activating the option `incremental.load`) with some changes to the program or refinements in the
configuration. The respective `asserts` and expected results are checked in both runs.

### Running
The incremental tests can be run with `./scripts/update_suite.rb -i`.

### Writing
An incremental test case consists of three files with the same file name: the `.c` file with the initial program, a
`.json` file for the initial configuration and a `.patch` file with the changes for the incremental run. Asserts and
expected results are encoded in the program as for other regression tests.

The patch file can contain changes to multiple files. This allows for modifications of code, asserts or expected results
in the program file as well as refinements in the configuration for the incremental run. A suitable patch can be created
with:
```
git diff --no-prefix relative/path/to/test.c relative/path/to/test.json > relative/path/to/test.patch
```

The comparison input and the metadata in the patch headers are not necessary and can be removed.

## Unit tests

### Running
The unit tests can be run with `dune runtest unittest`.
Use `--watch` for automatic rebuilding and retesting.

## Domain tests
Property-based testing (a la QuickCheck) is used to test some domains (`Lattice.S` implementations) for their lattice properties.
On integer domains the integer operations are also tested for being a valid abstraction of sets of integers.

### Running
Domain tests are now run as part of [unit tests](#unit-tests).

### Writing
To test a domain, you need to do the following:

1. Implement `arbitrary` (reasonably).
2. Add the domain to `Maindomaintest`.
