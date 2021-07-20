# Profiling

## perf
`perf` is a Linux profiling tool. It can be used to profile Goblint as follows:

1. Record profiling data from a Goblint run: `perf record ./goblint GOBLINT_ARGUMENTS`.
2. View recorded data: `perf report`.

    Following keys can be used for navigation:

    * Arrows for moving around.
    * `+` for expanding/collapsing one callstack level
    * `e` for expanding/collapsing all callstack levels

    Function names are mangled: they contain the top-level OCaml module name and function name (or just `fun` for lambdas).
    Due to inlining some callstack levels might be skipped, making it difficult to follow.
