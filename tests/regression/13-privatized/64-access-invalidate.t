Should have (safe) write access to id1 and magic2 invalidate to A:

  $ goblint --enable allglobs 64-access-invalidate.c
  [Error][Imprecise][Unsound] Function definition missing for magic2 (64-access-invalidate.c:16:3-16:12)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (64-access-invalidate.c:16:3-16:12)
  [Info][Imprecise] Invalidating expressions: & A, & id, & e (64-access-invalidate.c:16:3-16:12)
  [Error][Imprecise][Unsound] Function definition missing for magic1 (64-access-invalidate.c:12:3-12:11)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (64-access-invalidate.c:12:3-12:11)
  [Info][Imprecise] Invalidating expressions: & A, & id (64-access-invalidate.c:12:3-12:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
  [Success][Race] Memory location id (safe): (64-access-invalidate.c:4:11-4:13)
    write with [multi:false, thread:[main]] (conf. 110)  (exp: & *((pthread_t * __restrict  )(& id))) (64-access-invalidate.c:21:3-21:36)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
  [Error][Imprecise][Unsound] Function definition missing
