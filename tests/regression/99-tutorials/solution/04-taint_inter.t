  $ goblint --set "ana.activated[+]" taintSol --disable warn.imprecise --set "exp.extraspecials[+]" printInt ../04-taint_inter.c
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:52:5-52:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:58:5-58:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:38:5-38:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:43:5-43:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 30
    dead: 0
    total lines: 30
