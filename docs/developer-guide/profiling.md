# Profiling

## Timing
`Timing` is an OCaml module (based on CIL's `Stats`) that can be used to profile certain parts of code.

Wrap the function call to be profiled with `Timing.wrap`. For example, replace
```ocaml
f x y z
```
with
```ocaml
Timing.wrap "mything" (f x y) z
```
where `mything` should be replaced with a relevant name to be shown in the output.
Note that all but the last argument are partially applied to `f`.
The last argument is given separately for `Timing.wrap` to apply and measure.

Then run Goblint with `--enable dbg.timing.enabled` or `-v` (verbose) to see the timing stats under `Timings:`.

The timings are automatically presented as a tree which follows the nesting of `Timing.wrap` calls.
Unlike [tracing](./debugging.md#tracing), timings cannot be toggled easily individually, so be considerate of where you leave them after doing the profiling.

### Trace Event Format
[Trace Event Format (TEF)](https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/edit) is a simple JSON-based format for profiling data.
Besides just accumulating timing information, all timed calls are individually recorded, allowing flamegraph-style visualization.

Goblint can produce a TEF file with
```console
--enable dbg.timing.enabled --set dbg.timing.tef goblint.timing.json
```
Then open `goblint.timing.json` in [Perfetto UI](https://ui.perfetto.dev/).


## perf
`perf` is a Linux profiling tool.
First, record profiling data from a Goblint run:
```console
perf record --call-graph=dwarf ./goblint GOBLINT_ARGUMENTS
```

Then the resulting `perf.data` file can be inspected in various ways.

### perf report
`perf report` is its built-in terminal based viewing tool:

1. For top-down hierarchy run: `perf report --children`.
2. For bottom-up hierarchy run: `perf report --no-children`.

Following keys can be used for navigation:

* Arrows for moving around.
* `+` for expanding/collapsing one callstack level.
* `e` for expanding/collapsing all callstack levels.
* `q` for quitting.

Function names are mangled: they contain the top-level OCaml module name and function name (or just `fun` for lambdas).
Due to inlining some callstack levels might be skipped, making it difficult to follow.

### Firefox Profiler
[Firefox Profiler](https://profiler.firefox.com/) is a web-based tool, which also supports a form of perf output.
It's easier to browse around with and supports additional features like built-in flame graphs.

It can be used as follows:

1. Convert `perf.data` by running: `perf script -F +pid > OUTPUT_FILE`.
2. Go to <https://profiler.firefox.com/>.
3. Click "Load a profile from file" and upload `OUTPUT_FILE`.

For more information, see [Firefox Profiler's documentation](https://profiler.firefox.com/docs/#/./guide-perf-profiling).

An experimental version of Firefox Profiler, which supports collapsing _indirect_ recursion, can be found [here](https://deploy-preview-4232--perf-html.netlify.app/).


## Memtrace
`memtrace` is a library and tool for profiling OCaml memory usage.
For a tutorial, see [Jane Street Tech Blog](https://blog.janestreet.com/finding-memory-leaks-with-memtrace/).
