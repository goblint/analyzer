  $ goblint --enable dbg.debug 01-assert.c
  [Success][Assert] Assertion "success" will succeed (01-assert.c:9:3-9:18)
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:10:3-10:15)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:23)
  Pruning result
