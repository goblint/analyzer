The shutdown check fails after an OOM bailout and succeeds on a normal path.

  $ goblint --enable ana.int.interval --enable sem.malloc.fail --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 1 --set ana.activated[+] useAfterFree --enable warn.deterministic --disable warn.race 58-php.c
  [Info] Longjmp -> enabling longjmp analyses: "activeLongjmp, activeSetjmp, taintPartialContexts, modifiedSinceSetjmp, poisonVariables, expsplit, vla"
  [Error][Assert] Assertion "p->initialized" will fail. (58-php.c:71:9-71:40)
  [Success][Assert] Assertion "p->initialized" will succeed (58-php.c:71:9-71:40)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 0
    total lines: 29
  [Info][Unsound] Unknown value in ((alloc@sid:18@tid:[main]), 58-php.c:47:10-47:24) could be an escaped pointer address! (58-php.c:61:5-61:41)
