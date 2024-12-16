  $ goblint --enable warn.deterministic 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test ancient solvers:

  $ goblint --enable warn.deterministic --set solver WL 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver effectWConEq 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test topdown solvers:

  $ goblint --enable warn.deterministic --set solver topdown_deprecated 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown_term 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown_space_cache_term 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver td3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test SLR solvers:

  $ goblint --enable warn.deterministic --set solver widen1 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver widen2 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver widen3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver new 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr+ 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr1 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr2 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr4 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr1p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9
  $ goblint --enable warn.deterministic --set solver slr2p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr4p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3t 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3tp 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9
