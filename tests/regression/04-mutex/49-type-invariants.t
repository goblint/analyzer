  $ goblint --enable warn.deterministic --enable ana.race.direct-arithmetic --enable allglobs 49-type-invariants.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (49-type-invariants.c:21:3-21:21)
  [Warning][Race] Memory location s.field (race with conf. 110): (49-type-invariants.c:8:10-8:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
    read with [mhp:{tid=[main, t_fun@49-type-invariants.c:20:3-20:40#top]}, thread:[main, t_fun@49-type-invariants.c:20:3-20:40#top]] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (49-type-invariants.c:21:3-21:21)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (49-type-invariants.c:21:3-21:21)
  [Info][Imprecise] Invalidating expressions: & s (49-type-invariants.c:21:3-21:21)
  [Info][Imprecise] Invalidating expressions: & tmp (49-type-invariants.c:21:3-21:21)
  [Error][Imprecise][Unsound] Function definition missing for getS (49-type-invariants.c:21:3-21:21)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --disable ana.race.direct-arithmetic --enable allglobs 49-type-invariants.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (49-type-invariants.c:21:3-21:21)
  [Warning][Race] Memory location s.field (race with conf. 110): (49-type-invariants.c:8:10-8:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
    read with [mhp:{tid=[main, t_fun@49-type-invariants.c:20:3-20:40#top]}, thread:[main, t_fun@49-type-invariants.c:20:3-20:40#top]] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (49-type-invariants.c:21:3-21:21)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (49-type-invariants.c:21:3-21:21)
  [Info][Imprecise] Invalidating expressions: & s (49-type-invariants.c:21:3-21:21)
  [Info][Imprecise] Invalidating expressions: & tmp (49-type-invariants.c:21:3-21:21)
  [Error][Imprecise][Unsound] Function definition missing for getS (49-type-invariants.c:21:3-21:21)
  [Error][Imprecise][Unsound] Function definition missing
