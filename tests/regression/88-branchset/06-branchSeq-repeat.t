Consecutive identical decisions do not consume history slots: the first branch
still separates x and y after visiting the same node twice with the same outcome.
A different outcome at that node consumes a slot and evicts the first branch.

  $ goblint --set ana.activated[+] branchSeq --set ana.branchSeq.length 2 06-branchSeq-repeat.c
  [Success][Assert] Assertion "x == y" will succeed (06-branchSeq-repeat.c:22:3-22:26)
  [Warning][Assert] Assertion "x == y" is unknown. (06-branchSeq-repeat.c:24:3-24:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
