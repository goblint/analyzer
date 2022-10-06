Should have (safe) write accesses to id1 and id2:

  $ goblint --enable allglobs 63-access-threadspawn-lval.c
  [Error][Imprecise][Unsound] Function definition missing for magic2 (63-access-threadspawn-lval.c:21:3-21:12)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (63-access-threadspawn-lval.c:21:3-21:12)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(A,
                                       NoOffset)), AddrOf(Var(id2, NoOffset)), AddrOf(Var(id1, NoOffset)), AddrOf(Var(e, NoOffset)) (63-access-threadspawn-lval.c:21:3-21:12)
  [Error][Imprecise][Unsound] Function definition missing for magic1 (63-access-threadspawn-lval.c:13:3-13:11)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (63-access-threadspawn-lval.c:13:3-13:11)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(A, NoOffset)), AddrOf(Var(id2, NoOffset)), AddrOf(Var(id1, NoOffset)) (63-access-threadspawn-lval.c:13:3-13:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total: 13
  [Success][Race] Memory location id1@63-access-threadspawn-lval.c:4:11-4:14 (safe):
    write with [mhp:{tid=[main]},
              multi:false, thread:[main]] (conf. 110) (63-access-threadspawn-lval.c:27:3-27:37)
  [Success][Race] Memory location id2@63-access-threadspawn-lval.c:5:11-5:14 (safe):
    write with [mhp:{tid=[main]; created={[main, f@63-access-threadspawn-lval.c:27:3-27:37]}}, thread:[main]] (conf. 110) (63-access-threadspawn-lval.c:28:3-28:37)
    write with [mhp:{tid=[main]; created={[main, f@63-access-threadspawn-lval.c:27:3-27:37]}}, thread:[main]] (conf. 110) (63-access-threadspawn-lval.c:28:3-28:37)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total: 2
