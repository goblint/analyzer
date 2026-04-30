Run witness validation with `phaseGhost` where a worker ghost is advanced
twice in separate statements. The invariant after `pthread_create` is too
strong because the spawned worker may already have reached the third phase, but
current support is broken and still confirms it.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 07-sync-double-advance.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 07-sync-double-advance.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9
  [Warning][Witness] invariant unconfirmed: ghost_a == 0 || ghost_a == 1 (07-sync-double-advance.c:18:5)
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@07-sync-double-advance.c:15:5-15:51] and is only ever increased by one
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
