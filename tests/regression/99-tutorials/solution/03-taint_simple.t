  $ goblint --set "ana.activated[+]" taintSol --disable warn.imprecise --set "exp.extraspecials[+]" printInt ../03-taint_simple.c
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:18:5-18:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:19:5-19:22)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:20:5-20:24)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:23:5-23:20)
  [Warning][Integer > DivByZero][CWE-369] Second argument of division might be zero (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:29:5-29:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:30:5-30:20)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 41 (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:41-41)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 1
    total lines: 19
  [Warning][Deadcode][CWE-570] condition 'z == 0' is always false (/workspace_root/tests/regression/99-tutorials/03-taint_simple.c:40:8-40:14)
