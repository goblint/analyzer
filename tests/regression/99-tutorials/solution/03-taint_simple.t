  $ goblint --set "ana.activated[+]" taintSol --disable warn.imprecise --set "exp.extraspecials[+]" printInt ../03-taint_simple.c
  [Warning][Integer > DivByZero][CWE-369] Second argument of division might be zero (/workspace_root/default/tests/regression/99-tutorials/03-taint_simple.c:29:5-29:20)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 41 (/workspace_root/default/tests/regression/99-tutorials/03-taint_simple.c:41-41)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 1
    total lines: 19
  [Warning][Deadcode][CWE-570] condition 'z == 0' is always false (/workspace_root/default/tests/regression/99-tutorials/03-taint_simple.c:40:8-40:14)
