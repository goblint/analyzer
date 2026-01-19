  $ goblint --enable ana.sv-comp.functions 39-verifier-nondet-memory.c
  [Info][Imprecise] Invalidating expressions: (void *)(& x) (39-verifier-nondet-memory.c:18:3-18:44)
  [Warning][Assert] Assertion "x == 0" is unknown. (39-verifier-nondet-memory.c:19:3-19:26)
  [Info][Imprecise] Invalidating expressions: (void *)(& s) (39-verifier-nondet-memory.c:23:3-23:42)
  [Warning][Assert] Assertion "s.f == 0" is unknown. (39-verifier-nondet-memory.c:24:3-24:28)
  [Warning][Assert] Assertion "s.g == 0" is unknown. (39-verifier-nondet-memory.c:25:3-25:28)
  [Info][Imprecise] Invalidating expressions: (void *)values (39-verifier-nondet-memory.c:29:3-29:53)
  [Warning][Assert] Assertion "*(values + 0) == 0" is unknown. (39-verifier-nondet-memory.c:30:3-30:34)
  [Warning][Assert] Assertion "*(values + 1) == 0" is unknown. (39-verifier-nondet-memory.c:31:3-31:34)
  [Info][Imprecise] Invalidating expressions: (void *)(& p) (39-verifier-nondet-memory.c:35:3-35:45)
  [Warning][Assert] Assertion "(unsigned long )p == (unsigned long )(& x)" is unknown. (39-verifier-nondet-memory.c:36:3-36:27)
  [Success][Assert] Assertion "x == 0" will succeed (39-verifier-nondet-memory.c:37:3-37:26)
  [Info][Imprecise] Invalidating expressions: (void *)(& e) (39-verifier-nondet-memory.c:40:3-40:47)
  [Success][Assert] Assertion "1" will succeed (39-verifier-nondet-memory.c:43:5-43:23)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 22
    dead: 0
    total lines: 22
