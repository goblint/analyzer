Invalid witness: ghost updates assign wrong values (e.g. ghost set to 2 on unlock,
or unlocks left at 1). The mutexGhost analyzer should report that the ghost
variables are used incorrectly and refuse to confirm any related invariant.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 55-mutex-ghost-invalid-wrong-value.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 55-mutex-ghost-invalid-wrong-value.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 0
    total lines: 28
  [Info][Witness] mutexGhost: global ghost_1 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] mutexGhost: global ghost_2 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] mutexGhost: global ghost_3 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] mutexGhost: global ghost_4 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
