  $ goblint --set "ana.activated[+]" signsSol ../02-first-extend.c
  [Warning][Assert] Assertion "x < 1" is unknown. (/workspace_root/default/tests/regression/99-tutorials/02-first-extend.c:17:3-17:25)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

  $ goblint --set "ana.activated[+]" signsExtendSol ../02-first-extend.c
  [Warning][Assert] Assertion "x < 1" is unknown. (/workspace_root/default/tests/regression/99-tutorials/02-first-extend.c:17:3-17:25)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
