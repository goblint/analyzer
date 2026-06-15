Basic valid ghost instrumentation: two threads, two mutexes guarding two globals.
The invariant `ghost_1 == 1 || g1 == 0` at the end of main is confirmed because
every critical section that writes g1 leaves it at 0 before unlocking.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 50-mutex-ghost-valid-basic.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval_set --set ana.base.privatization mutex-meet-tid --set colors never 50-mutex-ghost-valid-basic.c 
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 0
    total lines: 28
  [Warning][Deadcode][CWE-570] condition 'g1 < 0' is always false (50-mutex-ghost-valid-basic.c:21:5-21:26)
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of all of the critical sections protected by mutex mutex1
  [Info][Witness] mutexGhost: global ghost_2 is only used to mark the boundary of all of the critical sections protected by mutex mutex2
  [Info][Witness] mutexGhost: global ghost_3 is only used to mark the boundary of all of the critical sections protected by mutex mutex2
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_1 == 1 || g1 == 0 (50-mutex-ghost-valid-basic.c:43:5)
