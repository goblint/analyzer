  $ goblint --enable allglobs 92-distribute-fields-type-deep.c
  [Error][Imprecise][Unsound] Function definition missing for getS (92-distribute-fields-type-deep.c:37:3-37:20)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (92-distribute-fields-type-deep.c:37:3-37:20)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (92-distribute-fields-type-deep.c:37:3-37:20)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (92-distribute-fields-type-deep.c:37:3-37:20)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (92-distribute-fields-type-deep.c:37:3-37:20)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (92-distribute-fields-type-deep.c:37:3-37:20)
  [Info][Unsound] Write to unknown address: privatization is unsound. (92-distribute-fields-type-deep.c:37:3-37:20)
  [Error][Imprecise][Unsound] Function definition missing for getU (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (92-distribute-fields-type-deep.c:45:3-45:17)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Unsound] Write to unknown address: privatization is unsound. (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Race] Memory location (struct T).s.field (safe):
    write with [mhp:{tid=[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]}, thread:[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]] (conf. 100)  (exp: & tmp->field) (92-distribute-fields-type-deep.c:37:3-37:20)
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]}, thread:[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]] (conf. 100)  (exp: & tmp->field) (92-distribute-fields-type-deep.c:37:3-37:20)
  [Warning][Race] Memory location (struct U).t.s.field (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]}, thread:[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]] (conf. 100)  (exp: & tmp->field) (92-distribute-fields-type-deep.c:37:3-37:20)
    write with [mhp:{tid=[main]; created={[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->t) (92-distribute-fields-type-deep.c:45:3-45:17)
  [Success][Race] Memory location (struct U).t (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@92-distribute-fields-type-deep.c:43:3-43:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->t) (92-distribute-fields-type-deep.c:45:3-45:17)
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 1
    total memory locations: 4
