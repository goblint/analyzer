Ghost instrumentation when mutexes are accessed through pointers.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 54-mutex-ghost-valid-ptr.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 54-mutex-ghost-valid-ptr.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 22
    dead: 0
    total lines: 22
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of all of the critical sections protected by mutex mutex1
  [Info][Witness] mutexGhost: global ghost_2 is only used to mark the boundary of all of the critical sections protected by mutex mutex2
  [Info][Witness] witness validation summary:
    confirmed: 2
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2
  [Success][Witness] invariant confirmed: ghost_1 == 1 || g1 == 37 (54-mutex-ghost-valid-ptr.c:35:3)
  [Success][Witness] invariant confirmed: ghost_2 == 1 || g2 == 42 (54-mutex-ghost-valid-ptr.c:35:3)
