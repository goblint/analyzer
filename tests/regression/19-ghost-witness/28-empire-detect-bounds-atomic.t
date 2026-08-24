Run witness validation on the original, unprotected empire example with atomic
`x`. Atomic accesses avoid data-race undefined behavior, but the increment and
the corresponding ghost update are not one indivisible operation. Therefore,
all three invariants must remain unconfirmed.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 28-empire-detect-bounds-atomic.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 28-empire-detect-bounds-atomic.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16
  [Warning][Witness] invariant unconfirmed: (ghost_b == 0 && x >= 1) || (ghost_b == 1 && x >= 2) (28-empire-detect-bounds-atomic.c:18:1)
  [Warning][Witness] invariant unconfirmed: ghost_a == 1 && x >= 2 (28-empire-detect-bounds-atomic.c:37:5)
  [Warning][Witness] invariant unconfirmed: ghost_b == 1 && x >= 2 (28-empire-detect-bounds-atomic.c:37:5)
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@28-empire-detect-bounds-atomic.c:27:5-27:51] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 3
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
