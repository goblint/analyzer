  $ goblint --enable warn.deterministic --enable allglobs 80-type-nested-fields-deep2.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (80-type-nested-fields-deep2.c:36:3-36:22)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (80-type-nested-fields-deep2.c:43:3-43:24)
  [Warning][Race] Memory location (struct U).t.s.field (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#top]}, thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#top]] (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
    write with [mhp:{tid=[main]; created={[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->t.s.field) (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct T).s.field (safe):
    write with [mhp:{tid=[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#top]}, thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#top]] (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Write to unknown address: privatization is unsound. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Imprecise] Invalidating expressions: & tmp (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Imprecise] Invalidating expressions: & tmp (80-type-nested-fields-deep2.c:43:3-43:24)
  [Error][Imprecise][Unsound] Function definition missing for getT (80-type-nested-fields-deep2.c:36:3-36:22)
  [Error][Imprecise][Unsound] Function definition missing for getU (80-type-nested-fields-deep2.c:43:3-43:24)
  [Error][Imprecise][Unsound] Function definition missing
