# Phase-crafted ghost witness tasks

This directory contains 50 pthread-only regression programs for phase ghost witnesses.
Each task is designed so Goblint is inconclusive without the witness, but succeeds once
`phaseGhost`/`phaseGhostSplit` uses the witness phase information.

The programs intentionally vary the surface shape: fixed loops over arrays and ranges,
while loops, bitmasks, XOR fingerprints, boolean toggles, struct-field relations,
array-indexed updates, two/three-worker setups, dynamically spawned ghost-free background workers,
pointer-based mutex locking, points-to assertions, and workers with one, two, or three
phase advances. Several files include larger real-world-style scaffolding where only a
few shared phase boundaries matter; those larger cases use lighter SV-COMP levels in
their Cram tests to keep runtime reasonable.

There are no `__VERIFIER_atomic_begin` or `__VERIFIER_atomic_end` calls; synchronization
is by pthread mutexes and joins only.
