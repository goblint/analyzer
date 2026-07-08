# Phase-crafted ghost witness tasks

This directory contains 50 pthread-only regression programs for phase ghost witnesses.
Each task is designed so Goblint is inconclusive without the witness, but succeeds once
`phaseGhost`/`phaseGhostSplit` uses the witness phase information.

The programs are grouped by feature so that the directory is more than a
renaming exercise:

- `01`-`20`: varied phase protocols over ledgers, queues, caches, sessions,
  replicas, media pipelines, snapshots, and schedulers. These cover arrays,
  structs, bitmasks, XOR fingerprints, boolean toggles, pointer-free arithmetic
  relations, two/three-worker setups, and workers with one, two, or three phase
  advances.
- `21`-`25`: SV-COMP-style nondeterministic setup. A bounded nondeterministic
  value controls how many ghost-free background workers are spawned and joined,
  with deliberately non-regular create/join order. The asserted phase facts are
  independent of those helper workers.
- `26`-`30`: real-C-style mutex indirection. Workers lock and unlock mutexes
  through pointer variables while updating shared records.
- `31`-`35`: points-to and nondeterministic loop bounds. Bounded
  `__VERIFIER_nondet_int()` values control worker loop counts, and the final
  assertions compare phase-sensitive values against pointer-derived expected
  values while also checking points-to facts.
- `36`-`45`: larger benchmark-shaped programs with real-world-ish preprocessing
  helpers and only a few phase boundaries. These use lighter SV-COMP levels in
  their Cram tests to keep runtime reasonable.
- `46`-`50`: mixed table protocols with range loops, masks, while loops, and
  three-worker update patterns.

There are no `__VERIFIER_atomic_begin` or `__VERIFIER_atomic_end` calls; synchronization
is by pthread mutexes and joins only.
