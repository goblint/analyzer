  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Should also work with earlyglobs.
Earlyglobs shouldn't cause protected writes in multithreaded mode from being immediately published to protected invariant.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable exp.earlyglobs 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Same with mutex-meet.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization mutex-meet 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Should also work with earlyglobs.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization mutex-meet --enable exp.earlyglobs 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
