Ghost instrumentation where lock and unlock are wrapped in their own functions.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 52-mutex-ghost-valid-func.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 52-mutex-ghost-valid-func.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of all of the critical sections protected by mutex mutex
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_1 == 1 || myglobal == 42 (52-mutex-ghost-valid-func.c:33:3)
