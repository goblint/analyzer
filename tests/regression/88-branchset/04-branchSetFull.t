  $ goblint --set ana.activated[+] branchSetFull --disable ana.base.context.int 04-branchSetFull.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Success][Assert] Assertion "x == y" will succeed (04-branchSetFull.c:7:3-7:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9

  $ goblint --set ana.activated[+] branchSet --disable ana.base.context.int 04-branchSetFull.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Warning][Assert] Assertion "x == y" is unknown. (04-branchSetFull.c:7:3-7:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9

  $ goblint --set ana.activated[+] branchSetLocal --disable ana.base.context.int 04-branchSetFull.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Warning][Assert] Assertion "x == y" is unknown. (04-branchSetFull.c:7:3-7:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9
