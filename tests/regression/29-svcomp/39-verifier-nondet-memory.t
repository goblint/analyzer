  $ goblint --enable ana.sv-comp.functions 39-verifier-nondet-memory.c
  [Warning][Imprecise] Trying to update an index, but the array is unknown (/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/src/sv-comp.c:55:9-55:41)
  [Warning][Assert] Assertion "x == 0" is unknown. (39-verifier-nondet-memory.c:19:3-19:26)
  [Info][Imprecise] Trying to read a field, but the struct is unknown (39-verifier-nondet-memory.c:24:3-24:28)
  [Warning][Assert] Assertion "s.f == 0" is unknown. (39-verifier-nondet-memory.c:24:3-24:28)
  [Info][Imprecise] Trying to read a field, but the struct is unknown (39-verifier-nondet-memory.c:25:3-25:28)
  [Warning][Assert] Assertion "s.g == 0" is unknown. (39-verifier-nondet-memory.c:25:3-25:28)
  [Warning][Assert] Assertion "*(values + 0) == 0" is unknown. (39-verifier-nondet-memory.c:30:3-30:34)
  [Warning][Assert] Assertion "*(values + 1) == 0" is unknown. (39-verifier-nondet-memory.c:31:3-31:34)
  [Warning][Assert] Assertion "(unsigned long )p == (unsigned long )(& x)" is unknown. (39-verifier-nondet-memory.c:36:3-36:27)
  [Success][Assert] Assertion "x == 0" will succeed (39-verifier-nondet-memory.c:37:3-37:26)
  [Info][Unsound] Unknown value in {&e} could be an escaped pointer address! (39-verifier-nondet-memory.c:40:3-40:47)
  [Success][Assert] Assertion "1" will succeed (39-verifier-nondet-memory.c:43:5-43:23)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 0
    total lines: 28
