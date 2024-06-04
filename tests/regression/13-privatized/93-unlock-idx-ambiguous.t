TODO: should not succeed

  $ goblint --set ana.base.privatization protection --enable ana.sv-comp.functions 93-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (93-unlock-idx-ambiguous.c:14:3-14:30)
  [Success][Assert] Assertion "g == 0" will succeed (93-unlock-idx-ambiguous.c:24:3-24:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

TODO: should not succeed

  $ goblint --set ana.base.privatization mutex-meet --enable ana.sv-comp.functions 93-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (93-unlock-idx-ambiguous.c:14:3-14:30)
  [Success][Assert] Assertion "g == 0" will succeed (93-unlock-idx-ambiguous.c:24:3-24:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

TODO: should not succeed

  $ goblint --set ana.base.privatization lock --enable ana.sv-comp.functions 93-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (93-unlock-idx-ambiguous.c:14:3-14:30)
  [Success][Assert] Assertion "g == 0" will succeed (93-unlock-idx-ambiguous.c:24:3-24:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

TODO: should not succeed

  $ goblint --set ana.base.privatization write --enable ana.sv-comp.functions 93-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (93-unlock-idx-ambiguous.c:14:3-14:30)
  [Success][Assert] Assertion "g == 0" will succeed (93-unlock-idx-ambiguous.c:24:3-24:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

TODO: should not succeed

  $ goblint --set ana.base.privatization mine-nothread --enable ana.sv-comp.functions 93-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (93-unlock-idx-ambiguous.c:14:3-14:30)
  [Success][Assert] Assertion "g == 0" will succeed (93-unlock-idx-ambiguous.c:24:3-24:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

