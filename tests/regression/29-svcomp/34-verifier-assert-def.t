  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval 34-verifier-assert-def.c
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow (34-verifier-assert-def.c:4:7-4:18)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8

  $ goblint --enable ana.sv-comp.functions --set exp.extraspecials[+] __VERIFIER_assert 34-verifier-assert-def.c
  [Info][Analyzer] Using special for defined function __VERIFIER_assert (34-verifier-assert-def.c:9:3-9:23)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (34-verifier-assert-def.c:9:3-9:23)
  [Info][Imprecise] Invalidating expressions: 1 (34-verifier-assert-def.c:9:3-9:23)
  [Info][Analyzer] Using special for defined function __VERIFIER_assert (34-verifier-assert-def.c:10:3-10:23)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (34-verifier-assert-def.c:10:3-10:23)
  [Info][Imprecise] Invalidating expressions: r (34-verifier-assert-def.c:10:3-10:23)
  [Info][Analyzer] Using special for defined function __VERIFIER_assert (34-verifier-assert-def.c:11:3-11:23)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (34-verifier-assert-def.c:11:3-11:23)
  [Info][Imprecise] Invalidating expressions: 0 (34-verifier-assert-def.c:11:3-11:23)
  [Warning][Deadcode] Function '__VERIFIER_assert' is uncalled: 2 LLoC (34-verifier-assert-def.c:3:1-5:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 2 (2 in uncalled functions)
    total lines: 7
