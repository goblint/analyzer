  $ goblint --enable ana.race.direct-arithmetic --enable allglobs 49-type-invariants.c
  [Error][Imprecise][Unsound] Function definition missing for getS (49-type-invariants.c:22:3-22:21)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (49-type-invariants.c:22:3-22:21)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (49-type-invariants.c:22:3-22:21)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (49-type-invariants.c:22:3-22:21)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (49-type-invariants.c:22:3-22:21)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (49-type-invariants.c:22:3-22:21)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (49-type-invariants.c:22:3-22:21)
  [Info][Unsound] Write to unknown address: privatization is unsound. (49-type-invariants.c:22:3-22:21)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Race] Memory location s.field@49-type-invariants.c:9:10-9:11 (race with conf. 110):
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:21:3-21:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:22:3-22:21)
    read with [mhp:{tid=[main, t_fun@49-type-invariants.c:21:3-21:40#top]}, thread:[main, t_fun@49-type-invariants.c:21:3-21:40#top]] (conf. 110)  (exp: & s.field) (49-type-invariants.c:12:3-12:23)
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:21:3-21:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:22:3-22:21)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2

  $ goblint --disable ana.race.direct-arithmetic --enable allglobs 49-type-invariants.c
  [Error][Imprecise][Unsound] Function definition missing for getS (49-type-invariants.c:22:3-22:21)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (49-type-invariants.c:22:3-22:21)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (49-type-invariants.c:22:3-22:21)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (49-type-invariants.c:22:3-22:21)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (49-type-invariants.c:22:3-22:21)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (49-type-invariants.c:22:3-22:21)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (49-type-invariants.c:22:3-22:21)
  [Info][Unsound] Write to unknown address: privatization is unsound. (49-type-invariants.c:22:3-22:21)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Race] Memory location s.field@49-type-invariants.c:9:10-9:11 (race with conf. 110):
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:21:3-21:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:22:3-22:21)
    read with [mhp:{tid=[main, t_fun@49-type-invariants.c:21:3-21:40#top]}, thread:[main, t_fun@49-type-invariants.c:21:3-21:40#top]] (conf. 110)  (exp: & s.field) (49-type-invariants.c:12:3-12:23)
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:21:3-21:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:22:3-22:21)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
