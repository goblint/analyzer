# Benchmarking
The following best practices should be followed when benchmarking Goblint.
This is to ensure valid, reproducible and representative benchmarking results.

# External benchmarking
External users should choose the version of Goblint to evaluate or benchmark as follows:

1. Use the newest version release.

    The version from git `master` branch or any other intermediate git commit come without any guarantees.
    They are bleeding-edge and haven't gone through validation like the version releases.

    SV-COMP releases are highly preferable since they've gone through rigorous validation in SV-COMP.

2. Download the corresponding version from a Zenodo artifact or by checking out the respective git tag. **Do not install directly from opam repository!**

    Goblint pins optimized versions of some dependencies which cannot be done on the opam repository releases.
    Thus, using the latter would yield unrepresentative results.

    Zenodo artifacts come with DOIs, which make them ideal for citation.

3. Use OCaml 4.14. **Do not use OCaml 5!**

    OCaml 5 has significant performance regressions, which yield unrepresentative benchmarking results.
    Goblint's `make setup` installs the correct OCaml version into a new opam switch.

# Release build
To achieve the best performance for benchmarking, Goblint should be compiled using the `release` option:

```sh
make release
```

Run `goblint --version` to see an overview of the version of Goblint and Cil in the compiled binary.
This information is useful to be able to identify which versions were used to obtain your benchmark results.
If the above command prints a "`-dirty`" suffix in the "`Goblint version`", this indicates that the analyzer repository was not clean when the binary was built.
The "`Profile:`" information that is shown should be "`release`", after building Goblint as described.
