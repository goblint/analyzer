Zero history bounds are rejected when the analysis is active.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.length 0 05-branchSeq.c 2>&1 | grep 'ana.branchSeq.length must be positive'
  Fatal error: exception Failure("ana.branchSeq.length must be positive if analysis is active")

With k = 1, the decision is preserved across returns and calls, but the next
branch evicts it.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.length 1 05-branchSeq.c
  [Success][Assert] Assertion "x == y" will succeed (05-branchSeq.c:8:3-8:26)
  [Warning][Assert] Assertion "x == y" is unknown. (05-branchSeq.c:27:3-27:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16

With k = 2, the original decision is still retained after another branch.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.length 2 05-branchSeq.c
  [Success][Assert] Assertion "x == y" will succeed (05-branchSeq.c:8:3-8:26)
  [Success][Assert] Assertion "x == y" will succeed (05-branchSeq.c:27:3-27:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16

Negative history bounds are rejected.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.length -1 05-branchSeq.c 2>&1 | grep 'ana.branchSeq.length must be positive if analysis is active'
  Fatal error: exception Failure("ana.branchSeq.length must be positive if analysis is active")
