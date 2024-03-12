  $ goblint --enable ana.sv-comp.functions 33-verifier-assert-undef.c
  [Error][Imprecise][Unsound] Function definition missing for __VERIFIER_assert (33-verifier-assert-undef.c:5:3-5:23)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (33-verifier-assert-undef.c:5:3-5:23)
  [Info][Imprecise] Invalidating expressions: 1 (33-verifier-assert-undef.c:5:3-5:23)
  [Error][Imprecise][Unsound] Function definition missing for __VERIFIER_assert (33-verifier-assert-undef.c:6:3-6:23)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (33-verifier-assert-undef.c:6:3-6:23)
  [Info][Imprecise] Invalidating expressions: r (33-verifier-assert-undef.c:6:3-6:23)
  [Error][Imprecise][Unsound] Function definition missing for __VERIFIER_assert (33-verifier-assert-undef.c:7:3-7:23)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (33-verifier-assert-undef.c:7:3-7:23)
  [Info][Imprecise] Invalidating expressions: 0 (33-verifier-assert-undef.c:7:3-7:23)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  [Error][Imprecise][Unsound] Function definition missing
