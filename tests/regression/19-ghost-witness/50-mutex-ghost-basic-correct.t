Check that values of non-phase ghost variables are carried over when phase
changes are propagated. Otherwise, the branch-local update in the new phase
would unsoundly confirm the invariant.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 50-mutex-ghost-basic-correct.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set ana.base.privatization mutex-meet-tid --set colors never 50-mutex-ghost-basic-correct.c --trace priv
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 0
    total lines: 28
  [Warning][Witness] invariant unconfirmed: ghost_1 == 1 || g1 == 0 (50-mutex-ghost-basic-correct.c:43:5)
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of all of the critical sections protected by mutex {&mutex1}
  [Info][Witness] mutexGhost: global ghost_2 is only used to mark the boundary of all of the critical sections protected by mutex {&mutex2}
  [Info][Witness] mutexGhost: global ghost_3 is only used to mark the boundary of all of the critical sections protected by mutex {&mutex2}
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
