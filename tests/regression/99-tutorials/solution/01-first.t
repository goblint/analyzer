  $ goblint --set "ana.activated[+]" signsSol ../01-first.c
  [Success][Assert] Assertion "x < 0" will succeed (/workspace_root/tests/regression/99-tutorials/01-first.c:17:3-17:25)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

  $ goblint --set "ana.activated[+]" signsExtendSol ../01-first.c
  [Success][Assert] Assertion "x < 0" will succeed (/workspace_root/tests/regression/99-tutorials/01-first.c:17:3-17:25)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
