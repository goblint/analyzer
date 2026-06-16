Ghost instrumentation across helper functions called with different mutex arguments.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 51-mutex-ghost-valid-munge.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 51-mutex-ghost-valid-munge.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 22
    dead: 0
    total lines: 22
  [Warning][Witness] invariant unconfirmed: ghost_1 == 1 || g1 == 42 (51-mutex-ghost-valid-munge.c:36:3)
  [Info][Witness] mutexGhost: global ghost_1 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] mutexGhost: global ghost_2 is only used to mark the boundary of all of the critical sections protected by mutex mutex3
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2
  [Success][Witness] invariant confirmed: ghost_2 == 1 || g2 == 37 (51-mutex-ghost-valid-munge.c:36:3)
