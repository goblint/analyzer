  $ goblint --set ana.base.privatization protection --enable ana.sv-comp.functions 94-unlock-unknown.c
  [Info][Unsound] Unknown mutex unlocked, privatization unsound (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (94-unlock-unknown.c:12:3-12:26)
  [Success][Assert] Assertion "g == 0" will succeed (94-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.base.privatization mutex-meet --enable ana.sv-comp.functions 94-unlock-unknown.c
  [Info][Unsound] Unknown mutex unlocked, privatization unsound (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (94-unlock-unknown.c:12:3-12:26)
  [Success][Assert] Assertion "g == 0" will succeed (94-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.base.privatization lock --enable ana.sv-comp.functions 94-unlock-unknown.c
  [Info][Unsound] Unknown mutex unlocked, privatization unsound (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (94-unlock-unknown.c:12:3-12:26)
  [Success][Assert] Assertion "g == 0" will succeed (94-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.base.privatization write --enable ana.sv-comp.functions 94-unlock-unknown.c
  [Info][Unsound] Unknown mutex unlocked, privatization unsound (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (94-unlock-unknown.c:12:3-12:26)
  [Success][Assert] Assertion "g == 0" will succeed (94-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.base.privatization mine-nothread --enable ana.sv-comp.functions 94-unlock-unknown.c
  [Info][Unsound] Unknown mutex unlocked, privatization unsound (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (94-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (94-unlock-unknown.c:12:3-12:26)
  [Success][Assert] Assertion "g == 0" will succeed (94-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

