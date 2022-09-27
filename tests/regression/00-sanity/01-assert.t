  $ goblint 01-assert.c
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  Live lines: 7
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  Found dead code on 2 lines!
  Total lines (logical LoC): 9
