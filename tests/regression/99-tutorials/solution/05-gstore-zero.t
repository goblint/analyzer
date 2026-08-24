  $ goblint --set ana.activated '["gStoreWideningSol","assert","escape"]' ../05-gstore-zero.c
  [Success][Assert] Assertion "x < 0" will succeed (/workspace_root/tests/regression/99-tutorials/05-gstore-zero.c:17:3-17:25)
  [Success][Assert] Assertion "x < 0" will succeed (/workspace_root/tests/regression/99-tutorials/05-gstore-zero.c:25:3-25:25)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 21 (/workspace_root/tests/regression/99-tutorials/05-gstore-zero.c:21-21)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 1
    total lines: 9
  [Warning][Deadcode][CWE-570] condition 'x > 8' is always false (/workspace_root/tests/regression/99-tutorials/05-gstore-zero.c:19:6-19:11)
