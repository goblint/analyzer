Should succeed with branchSetFull.

  $ goblint --set ana.activated[+] branchSetFull --disable ana.base.context.int 05-branchSet-context.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Success][Assert] Assertion "x == y" will succeed (05-branchSet-context.c:15:3-15:26)
  [Success][Assert] Assertion "x == y" will succeed (05-branchSet-context.c:11:3-11:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13

  $ goblint --set ana.activated[+] branchSetFull --set ana.ctx_insens[+] branchSetFull --disable ana.base.context.int 05-branchSet-context.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Success][Assert] Assertion "x == y" will succeed (05-branchSet-context.c:15:3-15:26)
  [Success][Assert] Assertion "x == y" will succeed (05-branchSet-context.c:11:3-11:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13

Should not succeed with branchSet.

  $ goblint --set ana.activated[+] branchSet --disable ana.base.context.int 05-branchSet-context.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Success][Assert] Assertion "x == y" will succeed (05-branchSet-context.c:15:3-15:26)
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSet-context.c:11:3-11:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13

  $ goblint --set ana.activated[+] branchSet --set ana.ctx_insens[+] branchSet --disable ana.base.context.int 05-branchSet-context.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSet-context.c:15:3-15:26)
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSet-context.c:11:3-11:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13

Should not succeed with branchSetLocal.

  $ goblint --set ana.activated[+] branchSetLocal --disable ana.base.context.int 05-branchSet-context.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Success][Assert] Assertion "x == y" will succeed (05-branchSet-context.c:15:3-15:26)
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSet-context.c:11:3-11:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13

  $ goblint --set ana.activated[+] branchSetLocal --set ana.ctx_insens[+] branchSetLocal --disable ana.base.context.int 05-branchSet-context.c
  [Warning] ana.base.context.interval implicitly disabled by ana.base.context.int
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSet-context.c:15:3-15:26)
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSet-context.c:11:3-11:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
