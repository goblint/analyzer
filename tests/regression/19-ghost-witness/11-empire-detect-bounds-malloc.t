Run witness validation with `phaseGhost` on the empire example, but with `x`
as a global pointer to a dynamically allocated integer.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 11-empire-detect-bounds-malloc.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 11-empire-detect-bounds-malloc.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@11-empire-detect-bounds-malloc.c:30:5-30:51] and is monotonically increased to known bounds
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
  [Success][Witness] invariant confirmed: (ghost_b == 0 && *x >= 1) || (ghost_b == 1 && *x >= 2) (11-empire-detect-bounds-malloc.c:19:1)
  [Success][Witness] invariant confirmed: ghost_a == 1 && *x >= 2 (11-empire-detect-bounds-malloc.c:40:5)
  [Success][Witness] invariant confirmed: ghost_b == 1 && *x >= 2 (11-empire-detect-bounds-malloc.c:40:5)
