Run witness validation with `phaseGhost` on the empire example and confirm the extra `fun`-exit ghost invariant (but without tid privatization, so we don't get the invariants for free)

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 05-empire-detect-bounds.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 05-empire-detect-bounds.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@05-empire-detect-bounds.c:30:5-30:51] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: (ghost_b == 0 && x >= 1) || (ghost_b == 1 && x >= 2) (05-empire-detect-bounds.c:21:1)
  [Success][Witness] invariant confirmed: ghost_a == 1 && x >= 2 (05-empire-detect-bounds.c:42:5)
  [Success][Witness] invariant confirmed: ghost_b == 1 && x >= 2 (05-empire-detect-bounds.c:42:5)
