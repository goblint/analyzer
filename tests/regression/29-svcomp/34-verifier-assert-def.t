  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval 34-verifier-assert-def.c
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in << (34-verifier-assert-def.c:4:7-4:18)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8

  $ goblint --enable ana.sv-comp.functions --set exp.extraspecials[+] __VERIFIER_assert 34-verifier-assert-def.c
  [Info][Analyzer] Using special for defined function __VERIFIER_assert (34-verifier-assert-def.c:9:3-9:23)
  [Success][Assert] Assertion "1" will succeed (34-verifier-assert-def.c:9:3-9:23)
  [Info][Analyzer] Using special for defined function __VERIFIER_assert (34-verifier-assert-def.c:10:3-10:23)
  [Warning][Assert] Assertion "r" is unknown. (34-verifier-assert-def.c:10:3-10:23)
  [Info][Analyzer] Using special for defined function __VERIFIER_assert (34-verifier-assert-def.c:11:3-11:23)
  [Error][Assert] Assertion "0" will fail. (34-verifier-assert-def.c:11:3-11:23)
  [Warning][Deadcode] Function '__VERIFIER_assert' is uncalled: 2 LLoC (34-verifier-assert-def.c:3:1-5:1)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on line 12 (34-verifier-assert-def.c:12-12)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 3 (2 in uncalled functions)
    total lines: 7
