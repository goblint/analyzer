# Benchmarking

To achieve reproducible builds and the best performance for benchmarking, it is recommended to compile Goblint using the `release` option:

```sh
make release
```

Run `goblint --version` to see an overview of the version of Goblint and Cil in the compiled binary.
This information is useful to be able to identify which versions were used to obtain your benchmark results.
If the above command prints a "`-dirty`" suffix in the "`Goblint version`", this indicates that the analyzer repository was not clean when the binary was built.
The "`Profile:`" information that is shown should be "`release`", after building Goblint as described.
