Should succeed with branchSetFull.

  $ goblint --set ana.activated[+] branchSetFull 03-branchSetLocal.c
  [Success][Assert] Assertion "x == y" will succeed (03-branchSetLocal.c:16:3-16:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9

Should succeed with branchSet.

  $ goblint --set ana.activated[+] branchSet 03-branchSetLocal.c
  [Success][Assert] Assertion "x == y" will succeed (03-branchSetLocal.c:16:3-16:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9

Should not succeed with branchSetLocal.

  $ goblint --set ana.activated[+] branchSetLocal 03-branchSetLocal.c
  [Warning][Assert] Assertion "x == y" is unknown. (03-branchSetLocal.c:16:3-16:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9
