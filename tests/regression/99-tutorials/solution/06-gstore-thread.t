  $ goblint --set ana.activated '["gStoreWideningSol","assert","base","mallocWrapper","escape"]' --set ana.base.privatization none --enable exp.globs_are_top ../06-gstore-thread.c
  [Success][Assert] Assertion "global < 200" will succeed (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:31:3-31:32)
  [Success][Assert] Assertion "global >= 0" will succeed (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:32:3-32:31)
  [Warning][Assert] Assertion "global == 42" is unknown. (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:40:3-40:32)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 28 (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:28-28)
  [Warning][Deadcode] Function 'thread' has dead code:
    on line 10 (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:10-10)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 2
    total lines: 18
  [Warning][Deadcode][CWE-570] condition 'global < 0' is always false (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:9:6-9:16)
  [Warning][Deadcode][CWE-570] condition 'global > 200' is always false (/workspace_root/default/tests/regression/99-tutorials/06-gstore-thread.c:27:6-27:18)
