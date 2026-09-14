Check that values of non-phase ghost variables are carried over when phase
changes are propagated. Otherwise, the branch-local update in the new phase
would unsoundly confirm the invariant.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 12-non-phase-ghost-propagation.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 12-non-phase-ghost-propagation.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Warning][Witness] phaseGhost: global ghost_non_phase is only accessed by unique thread [main], but is not monotonically increased to known bounds
  [Warning][Witness] invariant unconfirmed: ghost_non_phase == 10 (12-non-phase-ghost-propagation.c:45:5)
  [Info][Witness] phaseGhost: global ghost_phase is only accessed by unique thread [main, fun@12-non-phase-ghost-propagation.c:31:5-31:51] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
