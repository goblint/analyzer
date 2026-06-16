Run witness validation with `phaseGhostSplit` where two alternative phase
changes have the same origin. Phase 1 must not be treated as an origin for the
alternative jump to phase 3.
Without origin-sensitive phase changes, phase 1 would be recursively advanced
to phase 3 and carry the phase-1 value of `x`, making the invariant unconfirmed.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 17-origin-phase-change.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 17-origin-phase-change.c
  [Warning][Deadcode] Function 'main' does not return
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@17-origin-phase-change.c:22:5-22:44] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_a != 3 || x == 3 (17-origin-phase-change.c:24:5)
