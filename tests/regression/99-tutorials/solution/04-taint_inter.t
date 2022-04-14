  $ goblint --set "ana.activated[+]" taintSol --disable warn.imprecise --set "exp.extraspecials[+]" printInt ../04-taint_inter.c
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:52:5-52:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:58:5-58:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:38:5-38:20)
  [Warning][Unknown] Tainted variable reaches sink! (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:43:5-43:20)
  [Warning][Deadcode] Function 'benign' is uncalled: 1 LLoC (/workspace_root/default/tests/regression/99-tutorials/04-taint_inter.c:16:1-18:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 1 (1 in uncalled functions)
    total lines: 27
