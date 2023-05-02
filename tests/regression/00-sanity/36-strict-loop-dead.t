  $ goblint 36-strict-loop-dead.c
  [Warning][Deadcode] Function 'basic2' has dead code:
    on line 8 (36-strict-loop-dead.c:8-8)
    on line 11 (36-strict-loop-dead.c:11-11)
    on line 13 (36-strict-loop-dead.c:13-13)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 3
    total lines: 10
  [Warning][Deadcode][CWE-571] condition 'n < 0' is always true (36-strict-loop-dead.c:5:7-5:12)
