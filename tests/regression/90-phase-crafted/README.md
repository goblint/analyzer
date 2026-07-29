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
