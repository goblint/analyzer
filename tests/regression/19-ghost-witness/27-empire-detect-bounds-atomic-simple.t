Run witness validation on the atomic empire example and confirm that the ghost
assertions remain provable with the weaker `x >= 1` bound after splitting
evaluation from the instrumented atomic assignment.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 27-empire-detect-bounds-atomic-simple.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 27-empire-detect-bounds-atomic-simple.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@27-empire-detect-bounds-atomic-simple.c:27:5-27:51] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: (ghost_b == 0 && x >= 1) || (ghost_b == 1 && x >= 1) (27-empire-detect-bounds-atomic-simple.c:18:1)
  [Success][Witness] invariant confirmed: ghost_a == 1 && x >= 1 (27-empire-detect-bounds-atomic-simple.c:37:5)
  [Success][Witness] invariant confirmed: ghost_b == 1 && x >= 1 (27-empire-detect-bounds-atomic-simple.c:37:5)
