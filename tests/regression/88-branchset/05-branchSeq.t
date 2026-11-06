Zero history bounds are rejected when the analysis is active.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.k 0 05-branchSeq.c 2>&1 | grep 'ana.branchSeq.k must be positive'
  Fatal error: exception Failure("ana.branchSeq.k must be positive if analysis is active")

With k = 1, the decision is preserved across returns and calls, but the next
branch evicts it. Disable value contexts so they cannot preserve the correlation.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.k 1 --disable ana.base.context.int --disable ana.base.context.interval 05-branchSeq.c
  [Success][Assert] Assertion "x == y" will succeed (05-branchSeq.c:8:3-8:26)
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSeq.c:27:3-27:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16

With k = 2, the original decision is still retained after another branch.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.k 2 --disable ana.base.context.int --disable ana.base.context.interval 05-branchSeq.c
  [Success][Assert] Assertion "x == y" will succeed (05-branchSeq.c:8:3-8:26)
  [Success][Assert] Assertion "x == y" will succeed (05-branchSeq.c:27:3-27:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16

Negative history bounds are rejected.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.k -1 05-branchSeq.c 2>&1 | grep 'ana.branchSeq.k must be positive if analysis is active'
  Fatal error: exception Failure("ana.branchSeq.k must be positive if analysis is active")
