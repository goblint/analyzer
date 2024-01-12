  $ goblint --enable warn.deterministic --enable allglobs 93-distribute-fields-type-global.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (93-distribute-fields-type-global.c:13:3-13:29)
  [Warning][Race] Memory location s.field (race with conf. 110): (93-distribute-fields-type-global.c:8:10-8:11)
    read with [mhp:{tid=[main, t_fun@93-distribute-fields-type-global.c:20:3-20:40#top]}, thread:[main, t_fun@93-distribute-fields-type-global.c:20:3-20:40#top]] (conf. 100)  (exp: & tmp->field) (93-distribute-fields-type-global.c:13:3-13:29)
    write with [mhp:{tid=[main]; created={[main, t_fun@93-distribute-fields-type-global.c:20:3-20:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (93-distribute-fields-type-global.c:22:3-22:9)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
  [Success][Race] Memory location (struct S).field (safe):
    read with [mhp:{tid=[main, t_fun@93-distribute-fields-type-global.c:20:3-20:40#top]}, thread:[main, t_fun@93-distribute-fields-type-global.c:20:3-20:40#top]] (conf. 100)  (exp: & tmp->field) (93-distribute-fields-type-global.c:13:3-13:29)
  [Success][Race] Memory location s (safe): (93-distribute-fields-type-global.c:8:10-8:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@93-distribute-fields-type-global.c:20:3-20:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (93-distribute-fields-type-global.c:22:3-22:9)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (93-distribute-fields-type-global.c:13:3-13:29)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (93-distribute-fields-type-global.c:13:3-13:29)
  [Info][Imprecise] Invalidating expressions: & s (93-distribute-fields-type-global.c:13:3-13:29)
  [Info][Imprecise] Invalidating expressions: & tmp (93-distribute-fields-type-global.c:13:3-13:29)
  [Error][Imprecise][Unsound] Function definition missing for getS (93-distribute-fields-type-global.c:13:3-13:29)
  [Error][Imprecise][Unsound] Function definition missing
