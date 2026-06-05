  $ goblint --set ana.activated '["gStoreWideningSol","effectivelyLocalSol","assert","base","mallocWrapper","thread","threadid","escape"]' --set ana.base.privatization none --enable exp.globs_are_top ../07-gstore-mixed.c
  [Success][Assert] Assertion "thread_owned == 42" will succeed (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:11:3-11:38)
  [Success][Assert] Assertion "thread_owned == 11" will succeed (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:20:3-20:38)
  [Success][Assert] Assertion "global < 200" will succeed (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:38:3-38:32)
  [Success][Assert] Assertion "global >= 0" will succeed (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:39:3-39:31)
  [Success][Assert] Assertion "thread_owned >= 0" will succeed (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:40:3-40:37)
  [Warning][Assert] Assertion "global == 42" is unknown. (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:48:3-48:32)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 35 (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:35-35)
  [Warning][Deadcode] Function 'thread' has dead code:
    on line 14 (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:14-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 21
    dead: 2
    total lines: 23
  [Warning][Deadcode][CWE-570] condition 'global < 0' is always false (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:13:6-13:16)
  [Warning][Deadcode][CWE-570] condition 'global > 200' is always false (/workspace_root/tests/regression/99-tutorials/07-gstore-mixed.c:34:6-34:18)
