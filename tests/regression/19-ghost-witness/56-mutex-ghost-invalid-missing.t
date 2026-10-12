Invalid witness: several lock/unlock sites are missing their corresponding ghost
update, so the witness no longer describes the full mutex protocol. The mutexGhost
analyzer should detect the incomplete ghost trace.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 56-mutex-ghost-invalid-missing.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 56-mutex-ghost-invalid-missing.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 0
    total lines: 28
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of part (not all) of the critical sections protected by mutex mutex1
  [Info][Witness] mutexGhost: global ghost_2 is only used to mark the boundary of part (not all) of the critical sections protected by mutex mutex2
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
