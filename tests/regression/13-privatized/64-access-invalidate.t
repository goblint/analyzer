Should have (safe) write access to id1 and magic2 invalidate to A:

  $ goblint --enable allglobs 64-access-invalidate.c
  [Error][Imprecise][Unsound] Function definition missing for magic2 (64-access-invalidate.c:16:3-16:12)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (64-access-invalidate.c:16:3-16:12)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(A,
                                       NoOffset)), AddrOf(Var(id, NoOffset)), AddrOf(Var(e, NoOffset)) (64-access-invalidate.c:16:3-16:12)
  [Error][Imprecise][Unsound] Function definition missing for magic1 (64-access-invalidate.c:12:3-12:11)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (64-access-invalidate.c:12:3-12:11)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(A, NoOffset)), AddrOf(Var(id, NoOffset)) (64-access-invalidate.c:12:3-12:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total: 10
  [Success][Race] Memory location id@64-access-invalidate.c:4:11-4:13 (safe):
    write with [mhp:{tid=[main]},
              multi:false, thread:[main]] (conf. 110) (64-access-invalidate.c:21:3-21:36)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total: 1
