  $ goblint --enable allglobs 93-distribute-fields-type-global.c
  [Error][Imprecise][Unsound] Function definition missing for getS (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (93-distribute-fields-type-global.c:14:3-14:29)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Unsound] Write to unknown address: privatization is unsound. (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Race] Memory location s@93-distribute-fields-type-global.c:9:10-9:11 (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@93-distribute-fields-type-global.c:21:3-21:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (93-distribute-fields-type-global.c:23:3-23:9)
  [Warning][Race] Memory location s.field@93-distribute-fields-type-global.c:9:10-9:11 (race with conf. 110):
    read with [mhp:{tid=[main, t_fun@93-distribute-fields-type-global.c:21:3-21:40#top]}, thread:[main, t_fun@93-distribute-fields-type-global.c:21:3-21:40#top]] (conf. 100)  (exp: & tmp->field) (93-distribute-fields-type-global.c:14:3-14:29)
    write with [mhp:{tid=[main]; created={[main, t_fun@93-distribute-fields-type-global.c:21:3-21:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (93-distribute-fields-type-global.c:23:3-23:9)
  [Success][Race] Memory location (struct S).field (safe):
    read with [mhp:{tid=[main, t_fun@93-distribute-fields-type-global.c:21:3-21:40#top]}, thread:[main, t_fun@93-distribute-fields-type-global.c:21:3-21:40#top]] (conf. 100)  (exp: & tmp->field) (93-distribute-fields-type-global.c:14:3-14:29)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
