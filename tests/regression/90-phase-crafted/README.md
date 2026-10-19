# Phase-crafted ghost witness tasks

This directory contains 50 pthread-only regression programs for evaluating
phase ghost witnesses. Each task is inconclusive for Goblint
without witness guidance and provable when `phaseGhost`/`phaseGhostSplit` uses
the supplied witness phase information.

The suite is organized around different concurrent-programming features:

- `01`-`20`: compact phase protocols inspired by ledgers, queues, caches,
  sessions, replicas, media pipelines, snapshots, and schedulers. These tasks
  use arrays, structs, bitmasks, XOR fingerprints, boolean state, arithmetic
  relations, and two- or three-worker synchronization patterns with different
  numbers of phase boundaries.
- `21`-`25`: SV-COMP-style nondeterministic setup. A bounded nondeterministic
  value controls how many ghost-free background workers are spawned. The
  create/join order is intentionally non-uniform, while the checked facts depend
  only on the phase-guided producer/consumer threads.
- `26`-`30`: real-C-style mutex indirection. Workers lock and unlock mutexes
  through pointer variables while updating shared records and checking
  phase-sensitive facts.
- `31`-`35`: points-to and nondeterministic loop bounds. Bounded
  `__VERIFIER_nondet_int()` values control worker loop counts. The final
  assertions combine phase-sensitive values with pointer-derived expectations
  and explicit points-to checks.
- `36`-`45`: larger benchmark-shaped programs inspired by real-world data
  processing and service-maintenance code. These contain preprocessing helpers,
  extra control flow, and larger shared state, but only a small number of phase
  distinctions.
- `46`-`50`: compact protocols inspired by SCTBench and systems idioms such as
  work-stealing deque bookkeeping, seqlock publication, token-bucket throttling,
  priority inheritance, and RCU-style grace-period retirement.

Exactly half of the ordinary witnesses (`01`-`20` and `26`-`30`) also contain
location invariants. The remaining witnesses contain only ghost updates, so the
suite covers both pure phase guidance and combined phase/invariant validation.

All programs use pthreads directly. There are no `__VERIFIER_atomic_begin` or
`__VERIFIER_atomic_end` calls; synchronization is by pthread mutexes and joins.

Run all 50 tasks and print a runtime table from the repository root:

```sh
python3 tests/regression/90-phase-crafted/run-times.py
```

The script runs the baseline and witness commands from each task's `.t` file
sequentially (100 analyzer invocations), measuring wall-clock seconds. It saves
`runtimes.csv`, `runtimes.md`, and separate logs for each invocation under
`90-phase-crafted/result/runtimes/`. Existing files in that output directory are
overwritten. Build Goblint first, or select an executable with `--goblint PATH`.
Use `--output DIR` to change the output directory and `--timeout SECONDS` to
change the default 300-second limit per invocation. Timeouts and failed runs
remain in the table; the script continues through all tasks and exits nonzero
if any baseline result differs from `unknown` or witness result from `true`.

To include Ultimate verification without a witness and correctness-witness
validation for each task, run:

```sh
python3 tests/regression/90-phase-crafted/run-times-ultimate.py
```

The default Ultimate directory is
`/home/michael/Documents/papers/validating-concurrent-correctness-witnesses/ultimate/v.0.3.2/UGemCutter-linux`;
use `--ultimate-dir PATH` to override it.

The runner prefers the compatible Bitwuzla 0.9.1 build at
`result/tools/bitwuzla-0.9.1` in the repository root, if present; otherwise it
uses Ultimate's bundled solver. Use `--bitwuzla PATH` to select another build.
The local binary is an ignored build artifact and is not included in a fresh
checkout. The runner checks that the selected solver starts before benchmarking
and prints its version and path. This avoids the bundled binary's
`GLIBC_2.38 not found` failure on older systems. Ultimate runs in the output
directory's `ultimate-work/` subdirectory, with a `bitwuzla` symlink selecting
the solver even when Ultimate's launcher prepends its installation to `PATH`.

This runs four configurations per task (200 invocations), using the same
64-bit architecture as the suite's LP64 witnesses. Ultimate validation uses the
witness from the task's Goblint validation command and considers location
invariants as well as loop invariants. Both Ultimate modes expect `TRUE`.
The CSV and Markdown tables include separate timing and result columns for
all four configurations, with per-tool/per-mode logs, under
`90-phase-crafted/result/runtimes-ultimate/` by default. `--goblint`, `--output`,
and `--timeout` work as above; `--spec PATH` overrides the repository's
`unreach-call.prp`. A timeout terminates the tool's process group, including
Ultimate's Java process, and benchmarking continues with the next run.

To run up to 25 tasks concurrently and collate the results at the end:

```sh
tests/regression/90-phase-crafted/run-times-ultimate-par
```

Use `--jobs N` to choose a smaller concurrency limit (1–25, default 25).
Each task runs its four configurations sequentially, while different tasks
run concurrently. Task working directories and Ultimate runtime data are
isolated under the output directory to avoid shared temporary files.
The parallel runner accepts the same options and compatible Bitwuzla selection
as the sequential runner. Its default output directory is
`90-phase-crafted/result/runtimes-ultimate-par/`.
Progress goes to stderr; after all tasks finish, stdout contains the same
Markdown timing table, sorted by task number. `runtimes.csv`, `runtimes.md`,
and per-run logs are saved as before. Timeouts and failures appear in the table
and cause a nonzero exit status. Parallel timings include contention for the
machine's CPU and memory, so they are not directly comparable to sequential timings.
