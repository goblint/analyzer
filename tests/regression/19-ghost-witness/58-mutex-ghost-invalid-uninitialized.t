Ghost instrumentation with uninitialized mutex pointers.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 58-mutex-ghost-invalid-uninitialized.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 58-mutex-ghost-invalid-uninitialized.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Warning][Unknown] locking NULL mutex (58-mutex-ghost-invalid-uninitialized.c:8:3-8:24)
  [Warning][Unknown] unlocking NULL mutex (58-mutex-ghost-invalid-uninitialized.c:10:3-10:26)
  [Warning][Unknown] unlocking mutex ([__VERIFIER_atomic_instrument]) which may not be held (58-mutex-ghost-invalid-uninitialized.c:10:3-10:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (58-mutex-ghost-invalid-uninitialized.c:10:3-10:26)
  [Warning][Unknown] locking NULL mutex (58-mutex-ghost-invalid-uninitialized.c:19:3-19:24)
  [Warning][Unknown] unlocking NULL mutex (58-mutex-ghost-invalid-uninitialized.c:21:3-21:26)
  [Warning][Unknown] unlocking mutex ([__VERIFIER_atomic_instrument]) which may not be held (58-mutex-ghost-invalid-uninitialized.c:21:3-21:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (58-mutex-ghost-invalid-uninitialized.c:21:3-21:26)
  [Info][Witness] mutexGhost: global ghost_1 is used incorrectly, either matching several mutex, or non-mutex events
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
