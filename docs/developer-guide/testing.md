# Testing

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
