  $ goblint --enable warn.deterministic --enable allglobs 91-distribute-fields-type-2.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (91-distribute-fields-type-2.c:32:3-32:17)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (91-distribute-fields-type-2.c:40:3-40:17)
  [Warning][Race] Memory location (struct T).s (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@91-distribute-fields-type-2.c:38:3-38:40#top]}, thread:[main, t_fun@91-distribute-fields-type-2.c:38:3-38:40#top]] (conf. 100)  (exp: & *tmp) (91-distribute-fields-type-2.c:32:3-32:17)
    write with [mhp:{tid=[main]; created={[main, t_fun@91-distribute-fields-type-2.c:38:3-38:40#top]}}, thread:[main]] (conf. 100)  (exp: & *tmp) (91-distribute-fields-type-2.c:40:3-40:17)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
  [Success][Race] Memory location (struct S) (safe):
    write with [mhp:{tid=[main, t_fun@91-distribute-fields-type-2.c:38:3-38:40#top]}, thread:[main, t_fun@91-distribute-fields-type-2.c:38:3-38:40#top]] (conf. 100)  (exp: & *tmp) (91-distribute-fields-type-2.c:32:3-32:17)
  [Success][Race] Memory location (struct T) (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@91-distribute-fields-type-2.c:38:3-38:40#top]}}, thread:[main]] (conf. 100)  (exp: & *tmp) (91-distribute-fields-type-2.c:40:3-40:17)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (91-distribute-fields-type-2.c:32:3-32:17)
  [Info][Unsound] Write to unknown address: privatization is unsound. (91-distribute-fields-type-2.c:40:3-40:17)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (91-distribute-fields-type-2.c:32:3-32:17)
  [Info][Imprecise] Invalidating expressions: & tmp (91-distribute-fields-type-2.c:32:3-32:17)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (91-distribute-fields-type-2.c:40:3-40:17)
  [Info][Imprecise] Invalidating expressions: & tmp (91-distribute-fields-type-2.c:40:3-40:17)
  [Error][Imprecise][Unsound] Function definition missing for getS (91-distribute-fields-type-2.c:32:3-32:17)
  [Error][Imprecise][Unsound] Function definition missing for getT (91-distribute-fields-type-2.c:40:3-40:17)
  [Error][Imprecise][Unsound] Function definition missing
