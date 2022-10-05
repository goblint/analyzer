  $ goblint --enable ana.dead-code.lines --enable ana.dead-code.branches issue-94.c
  [Error][Assert] Assertion "tmp" will fail. (issue-94.c:13:3-13:35)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 8 (issue-94.c:8-8)
    on line 12 (issue-94.c:12-12)
    on line 14 (issue-94.c:14-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 3
    total: 9
  [Warning][Deadcode][CWE-571] condition '1' is always true (issue-94.c:5:7-5:8)
  [Warning][Deadcode][CWE-571] condition 'x' is always true (issue-94.c:9:7-9:8)
  [Warning][Deadcode][CWE-570] condition 'x > 1' (possibly inserted by CIL) is always false (issue-94.c:13:3-13:35)
