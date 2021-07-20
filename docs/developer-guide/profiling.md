# Profiling

## Stats
`Stats` is an OCaml module from CIL that can be used to profile certain parts of code.

Wrap the function call to be profiled with `Stats.time`. For example, replace
```ocaml
f x y z
```
with
```ocaml
Stats.time "mything" (f x y) z
```
where `mything` should be replaced with a relevant name to be shown in the output.
Note that all but the last argument are partially applied to `f`.
The last argument is given separately for `Stats.time` to apply and measure.

Then run Goblint with `--enable printstats` or `-v` (verbose) to see the timing stats under `Timings:`.

The timings are automatically presented as a tree which follows the nesting of `Stats.time` calls.
Unlike [tracing](./debugging.md#tracing), timings cannot be toggled easily, so be considerate of where you leave them after doing the profiling.


## perf
`perf` is a Linux profiling tool. It can be used to profile Goblint as follows:

1. Record profiling data from a Goblint run: `perf record ./goblint GOBLINT_ARGUMENTS`.
2. View recorded data: `perf report`.

    Following keys can be used for navigation:

    * Arrows for moving around.
    * `+` for expanding/collapsing one callstack level.
    * `e` for expanding/collapsing all callstack levels.
    * `q` for quitting.

    Function names are mangled: they contain the top-level OCaml module name and function name (or just `fun` for lambdas).
    Due to inlining some callstack levels might be skipped, making it difficult to follow.
