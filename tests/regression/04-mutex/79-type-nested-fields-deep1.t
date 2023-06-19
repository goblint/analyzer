  $ goblint --enable allglobs 79-type-nested-fields-deep1.c
  [Error][Imprecise][Unsound] Function definition missing for getS (79-type-nested-fields-deep1.c:37:3-37:20)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (79-type-nested-fields-deep1.c:37:3-37:20)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (79-type-nested-fields-deep1.c:37:3-37:20)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (79-type-nested-fields-deep1.c:37:3-37:20)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (79-type-nested-fields-deep1.c:37:3-37:20)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (79-type-nested-fields-deep1.c:37:3-37:20)
  [Info][Unsound] Write to unknown address: privatization is unsound. (79-type-nested-fields-deep1.c:37:3-37:20)
  [Error][Imprecise][Unsound] Function definition missing for getU (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (79-type-nested-fields-deep1.c:44:3-44:24)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Unsound] Write to unknown address: privatization is unsound. (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Race] Memory location (struct T).s.field (safe):
    write with [mhp:{tid=[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]}, thread:[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]] (conf. 100)  (exp: & tmp->field) (79-type-nested-fields-deep1.c:37:3-37:20)
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]}, thread:[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]] (conf. 100)  (exp: & tmp->field) (79-type-nested-fields-deep1.c:37:3-37:20)
  [Warning][Race] Memory location (struct U).t.s.field (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]}, thread:[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]] (conf. 100)  (exp: & tmp->field) (79-type-nested-fields-deep1.c:37:3-37:20)
    write with [mhp:{tid=[main]; created={[main, t_fun@79-type-nested-fields-deep1.c:43:3-43:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->t.s.field) (79-type-nested-fields-deep1.c:44:3-44:24)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
