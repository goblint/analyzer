Ghost instrumentation where locking is guarded by a nondeterministic branch.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 53-mutex-ghost-valid-branch.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 53-mutex-ghost-valid-branch.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of all of the critical sections protected by mutex m
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_1 == 1 || glob == 42 (53-mutex-ghost-valid-branch.c:36:3)
