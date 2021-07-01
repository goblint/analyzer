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
gobopt='--sets exp.privatization write+lock' ./scripts/update_suite.rb
```

### Annotating tests
* If you introduce a new warning message, you need to manually add it to `./scripts/update_suite.rb` in the declaration at https://github.com/goblint/analyzer/blob/a507a97148a4c4aa2d39eaf0e5c339c582a275a1/scripts/update_suite.rb#L386-L402, otherwise it won't be recognized as a warning
    * Example: ` when /Array out of bound/        then "warn"`
* Add parameters to a regression test in the first line: `//PARAM: --set dbg.debug true`
* Annotate lines inside the regression test with comments: `arr[9] = 10; //WARN` 

## Domain tests
Property-based testing (a la QuickCheck) is used to test some domains (`Lattice.S` implementations) for their lattice properties.
On integer domains the integer operations are also tested for being a valid abstraction of sets of integers.

### Running
1. Compile: `make domaintest`.
2. Run: `./goblint.domaintest`.

    See `--help` for other useful flags provided by qcheck, e.g. `-v` or `--long`.

### Writing
To test a domain, you need to do the following:

1. Implement `arbitrary` (reasonably).
2. Add the domain to `Maindomaintest`.
