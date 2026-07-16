# Testing

### Running

* Run _all_ tests with: `make test` or `dune runtest`.

    This runs all regression tests, cram tests, incremental tests and unit tests.

## Regression tests
Regression tests are small programs that can be used to quickly verify that existing functionality hasn't been broken.
They can be found in `./tests/regression/`.
Options that should be passed to Goblint when executing a regression test are specified in the test file using a single line comment starting with `//PARAM:`.

### Running
Regression tests can be run with various granularity:

* Run all (non-Apron) regression tests with: `./scripts/update_suite.rb`.
* Run all Apron tests with: `dune build @runaprontest`.
* Run a group of tests (by directory, without number) with: `./scripts/update_suite.rb group sanity`.

    Unfortunately this also runs skipped tests.
    This is a bug that is used as a feature in the tests with Apron, as not all CI jobs have the Apron library installed.

* Run a single test (by name, without group or number) with: `./scripts/update_suite.rb assert`.

    This compares Goblint output with test annotations (described below) and only outputs mismatches (i.e. test failures).
    It is useful for checking if the test passes (or which parts don't).
    Since group name is not specified, beware of same test name in multiple groups.

* Run a single test (by group and test number) with full output: `./regtest.sh 00 01`.

    This _does not_ compare Goblint output with test annotations, but directly shows all Goblint output.
    It is useful for debugging the test.
    Additional command-line options are also passed to Goblint.

To pass additional command-line options to Goblint with `update_suite.rb`, use the `gobopt` environment variable, e.g.:
```
gobopt='--set ana.base.privatization write+lock' ./scripts/update_suite.rb
```

### Writing
Regression tests use single-line comments (with `//`) as annotations.

#### First line
A comment on the first line can contain the following:

| Annotation | Comment |
| ---------- | ------- |
| `PARAM: ` <br> (NB! space) | The following command line parameters are added to Goblint for this test. |
| `SKIP` | The test is skipped (except when run with `./scripts/update_suite.rb group`). |
| `NOMARSHAL` | Marshaling and unmarshaling of results is not tested on this program. |

#### End of line
Comments at the end of other lines indicate the behavior on that line:

| Annotation | Expected Goblint result | Concrete semantics | Checks |
| ---------- | ----- | ------------- | --- |
| `SUCCESS` <br> or nothing | Assertion succeeds | Assertion always succeeds | Precision |
| `FAIL` | Assertion fails | Assertion always fails | Precision |
| `UNKNOWN!` | Assertion is unknown | Assertion may both <br> succeed or fail | Soundness |
| `UNKNOWN` | Assertion is unknown | — | Intended imprecision |
| `TODO` <br> or `SKIP` | Assertion is unknown <br> or succeeds | Assertion always succeeds | Precision improvement |
| `NORACE` | No race warning | No data race | Precision |
| `RACE!` | Race warning | Data race is possible | Soundness |
| `RACE` | Race warning | — | Intended imprecision |
| `NODEADLOCK` | No deadlock warning | No deadlock | Precision |
| `DEADLOCK` | Deadlock warning | Deadlock is possible | Soundness |
| `NOWARN` | No warning | — | Precision |
| `WARN` | Some warning | — | Soundness |

#### Other
Other useful constructs are the following:

| Code with annotation | Comment |
| -------------------- | ------- |
| `__goblint_check(1); // reachable` | Checks that the line is reachable according <br> to Goblint results (soundness). |
| `__goblint_check(0); // NOWARN (unreachable)` | Checks that the line is unreachable (precision). |

#### Meta
Comments at the end of lines can also indicate metaproperties:

| Annotation | Expected result/comment |
| ---------- | ----- |
| `NOCRASH` | No analyzer crash |
| `FIXPOINT` | No fixpoint error |
| `NOTIMEOUT` | Analyer terminates |
| `CRAM` | Automatic checks are only in corresponding Cram test |

These comments only document the intention of the test (if there are no other checks in the test).
Analyzer crash, fixpoint error and non-termination are checked even when there are other checks.

## Cram Tests
[Cram-style tests](https://dune.readthedocs.io/en/stable/tests.html#cram-tests) are also used to verify that existing functionality hasn't been broken.
They check the complete standard output of running the Goblint binary with specified command-line arguments.
Unlike regular regression tests, cram tests are not limited to testing annotations in C code.
They can be used to test arbitrary output from Goblint, such as program transformations.

Cram tests are located next to regression tests in `./tests/regression/`.

### Running
Cram tests can be run with various granularity:

* Run all cram tests with: `dune build @runcramtest`.
* Run cram tests in directory (e.g. `00-sanity`) with:

    ```
    dune build @tests/regression/00-sanity/runcramtest
    ```.

* Run a single cram test (e.g. `01-assert.t`) with:

    ```
    dune runtest tests/regression/00-sanity/01-assert.t
    ```.

### Writing
Create new cram tests in a subdirectory of `tests/regression` with the extension `.t`. The basic syntax of a cram test is as follows:

```cram
Anything not indented by two spaces is a comment.
  $ goblint <options...> file.c  # This command gets run in a shell.
  <This is the expected output of running the command.>
```

A `dune` file in the subdirectory must declare dependencies on other files, e.g. C files for goblint.
For example, to declare a dependency on all C and JSON files in the directory, use the `deps` stanza with `glob_files`:

```dune
(cram
 (deps (glob_files *.c) (glob_files *.json)))
```

The [Dune documentation on file tests](https://dune.readthedocs.io/en/stable/tests.html#file-tests) contains more details.

### Promoting Changes
When Goblint's output is intentionally changed by code changes, cram tests will fail.
After checking that the changes to Goblint's output shown in failing cram tests are as expected, you must update those tests.
Dune can automatically update cram test files, i.e. promote the changes.

First, run the offending test as above. Once the new output is correct:

* `dune promote` promotes the changes for all files.
* `dune promote <path...>` promotes the changes for the specified files and directories.

## Incremental tests
The incremental tests are regression tests that are first run with the option `incremental.save` and then again
incrementally (activating the option `incremental.load`) with some changes to the program or refinements in the
configuration. The respective `asserts` and expected results are checked in both runs.

### Running
Incremental tests can be run with various granularity:

* Run all incremental tests with: `dune runtest tests/incremental`.
* Run incremental tests using AST change detection with: `./scripts/update_suite.rb -i`.
* Run incremental tests using CFG change detection with: `./scripts/update_suite.rb -c`.

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
The unit tests can be run with `dune runtest tests/unit`.
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

## Coverage

The [bisect_ppx](https://github.com/aantron/bisect_ppx) tool is used to produce code coverage reports for Goblint.
The code coverage reports are available on [Coveralls](https://coveralls.io/github/goblint/analyzer).

To run `bisect_ppx` locally:

1. Install bisect_ppx with `opam install bisect_ppx`.
2. Run `make coverage` to build Goblint with bisect_ppx instrumentation.
3. Run tests with coverage: `mkdir -p _build/coverage ; BISECT_FILE="$(pwd)/_build/coverage/bisect" dune runtest --force --instrument-with bisect_ppx` (this will generate `.coverage` files into the directory `_build/coverage`).
4. Generate coverage report with `bisect-ppx-report html`.  
  Note: after that the generated `.coverage` files can be removed (e.g., with `find . -type f -name '*.coverage' -delete` or by deleting `_build/coverage`).
6. The HTML report can be found in the `_coverage` folder.
