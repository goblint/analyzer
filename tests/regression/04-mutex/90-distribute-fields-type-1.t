  $ goblint --enable warn.deterministic --enable allglobs 90-distribute-fields-type-1.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (90-distribute-fields-type-1.c:32:3-32:20)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (90-distribute-fields-type-1.c:40:3-40:17)
  [Warning][Race] Memory location (struct T).s.field (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@90-distribute-fields-type-1.c:38:3-38:40#top]}, thread:[main, t_fun@90-distribute-fields-type-1.c:38:3-38:40#top]] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:32:3-32:20)
    write with [mhp:{tid=[main]; created={[main, t_fun@90-distribute-fields-type-1.c:38:3-38:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:40:3-40:17)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main, t_fun@90-distribute-fields-type-1.c:38:3-38:40#top]}, thread:[main, t_fun@90-distribute-fields-type-1.c:38:3-38:40#top]] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:32:3-32:20)
  [Success][Race] Memory location (struct T).s (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@90-distribute-fields-type-1.c:38:3-38:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:40:3-40:17)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Unknown address in {&tmp} has escaped. (90-distribute-fields-type-1.c:32:3-32:20)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (90-distribute-fields-type-1.c:32:3-32:20)
  [Info][Unsound] Write to unknown address: privatization is unsound. (90-distribute-fields-type-1.c:32:3-32:20)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (90-distribute-fields-type-1.c:40:3-40:17)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (90-distribute-fields-type-1.c:40:3-40:17)
  [Info][Unsound] Write to unknown address: privatization is unsound. (90-distribute-fields-type-1.c:40:3-40:17)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (90-distribute-fields-type-1.c:32:3-32:20)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (90-distribute-fields-type-1.c:32:3-32:20)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (90-distribute-fields-type-1.c:40:3-40:17)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (90-distribute-fields-type-1.c:40:3-40:17)
  [Error][Imprecise][Unsound] Function definition missing for getS (90-distribute-fields-type-1.c:32:3-32:20)
  [Error][Imprecise][Unsound] Function definition missing for getT (90-distribute-fields-type-1.c:40:3-40:17)
