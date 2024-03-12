  $ goblint --enable ana.sv-comp.functions 33-verifier-assert-undef.c
  [Success][Assert] Assertion "1" will succeed (33-verifier-assert-undef.c:5:3-5:23)
  [Warning][Assert] Assertion "r" is unknown. (33-verifier-assert-undef.c:6:3-6:23)
  [Error][Assert] Assertion "0" will fail. (33-verifier-assert-undef.c:7:3-7:23)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on line 8 (33-verifier-assert-undef.c:8-8)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 1
    total lines: 5
