  $ goblint --set exp.single-threaded true 32-single-thr.c
  [Error][Imprecise][Unsound] Function definition missing for no_spawn (32-single-thr.c:12:3-12:15)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (32-single-thr.c:12:3-12:15)
  [Info][Imprecise] Invalidating expressions: & g, (void *)(& f) (32-single-thr.c:12:3-12:15)
  [Error][Unsound] Thread not spawned from f (32-single-thr.c:12:3-12:15)
  [Warning][Deadcode] Function 'f' is uncalled: 2 LLoC (32-single-thr.c:6:1-8:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 2 (2 in uncalled functions)
    total lines: 6
  [Error][Unsound] Thread not spawned
  [Error][Imprecise][Unsound] Function definition missing
