Invalid witness: ghost updates are added at sites that are not lock or unlock
operations, plus a spurious ghost_4 variable. The mutexGhost analyzer should
report the extra updates as misuse.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 57-mutex-ghost-invalid-extra.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 57-mutex-ghost-invalid-extra.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 0
    total lines: 28
  [Warning][Witness] mutexGhost: global ghost_1 is used incorrectly, either matching several mutex, or non-mutex events
  [Warning][Witness] mutexGhost: global ghost_2 is used incorrectly, either matching several mutex, or non-mutex events
  [Warning][Witness] mutexGhost: global ghost_3 is used incorrectly, either matching several mutex, or non-mutex events
  [Warning][Witness] mutexGhost: global ghost_4 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
