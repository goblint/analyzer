  $ goblint --enable warn.deterministic --enable allglobs 77-type-nested-fields.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (77-type-nested-fields.c:31:3-31:20)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (77-type-nested-fields.c:38:3-38:22)
  [Warning][Race] Memory location (struct T).s.field (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@77-type-nested-fields.c:37:3-37:40#top]}, thread:[main, t_fun@77-type-nested-fields.c:37:3-37:40#top]] (conf. 100)  (exp: & tmp->field) (77-type-nested-fields.c:31:3-31:20)
    write with [mhp:{tid=[main]; created={[main, t_fun@77-type-nested-fields.c:37:3-37:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->s.field) (77-type-nested-fields.c:38:3-38:22)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main, t_fun@77-type-nested-fields.c:37:3-37:40#top]}, thread:[main, t_fun@77-type-nested-fields.c:37:3-37:40#top]] (conf. 100)  (exp: & tmp->field) (77-type-nested-fields.c:31:3-31:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (77-type-nested-fields.c:31:3-31:20)
  [Info][Unsound] Write to unknown address: privatization is unsound. (77-type-nested-fields.c:38:3-38:22)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (77-type-nested-fields.c:31:3-31:20)
  [Info][Imprecise] Invalidating expressions: & tmp (77-type-nested-fields.c:31:3-31:20)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (77-type-nested-fields.c:38:3-38:22)
  [Info][Imprecise] Invalidating expressions: & tmp (77-type-nested-fields.c:38:3-38:22)
  [Error][Imprecise][Unsound] Function definition missing for getS (77-type-nested-fields.c:31:3-31:20)
  [Error][Imprecise][Unsound] Function definition missing for getT (77-type-nested-fields.c:38:3-38:22)
  [Error][Imprecise][Unsound] Function definition missing
