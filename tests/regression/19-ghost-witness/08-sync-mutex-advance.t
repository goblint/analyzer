Run witness validation with `phaseGhost` where the worker ghost advance is
inside the same mutex which main holds at the checked statement.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 08-sync-mutex-advance.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 08-sync-mutex-advance.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@08-sync-mutex-advance.c:21:5-21:51] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_a == 0 (08-sync-mutex-advance.c:23:5)
