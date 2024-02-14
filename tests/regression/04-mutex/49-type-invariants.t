  $ goblint --enable warn.deterministic --enable ana.race.direct-arithmetic --enable allglobs 49-type-invariants.c 2>&1 | tee default-output-1.txt
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (49-type-invariants.c:21:3-21:21)
  [Warning][Race] Memory location s.field (race with conf. 110): (49-type-invariants.c:8:10-8:11)
    write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
    read with thread:[main, t_fun@49-type-invariants.c:20:3-20:40] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
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

  $ goblint --enable warn.deterministic --disable ana.race.direct-arithmetic --enable allglobs 49-type-invariants.c 2>&1 | tee default-output-2.txt
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (49-type-invariants.c:21:3-21:21)
  [Warning][Race] Memory location s.field (race with conf. 110): (49-type-invariants.c:8:10-8:11)
    write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
    read with thread:[main, t_fun@49-type-invariants.c:20:3-20:40] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct S).field (safe):
    write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
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

  $ goblint --enable warn.deterministic --enable ana.race.direct-arithmetic --enable allglobs --enable dbg.full-output 49-type-invariants.c > full-output-1.txt 2>&1

  $ diff default-output-1.txt full-output-1.txt
  3,4c3,4
  <   write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  <   read with thread:[main, t_fun@49-type-invariants.c:20:3-20:40] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  ---
  >   write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  >   read with [mhp:{tid=[main, t_fun@49-type-invariants.c:20:3-20:40#top]}, thread:[main, t_fun@49-type-invariants.c:20:3-20:40#top]] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  11c11
  <   write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  ---
  >   write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  [1]

  $ goblint --enable warn.deterministic --disable ana.race.direct-arithmetic --enable allglobs --enable dbg.full-output 49-type-invariants.c > full-output-2.txt 2>&1

  $ diff default-output-2.txt full-output-2.txt
  3,4c3,4
  <   write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  <   read with thread:[main, t_fun@49-type-invariants.c:20:3-20:40] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  ---
  >   write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  >   read with [mhp:{tid=[main, t_fun@49-type-invariants.c:20:3-20:40#top]}, thread:[main, t_fun@49-type-invariants.c:20:3-20:40#top]] (conf. 110)  (exp: & s.field) (49-type-invariants.c:11:3-11:23)
  11c11
  <   write with [mhp:{created={[main, t_fun@49-type-invariants.c:20:3-20:40]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  ---
  >   write with [mhp:{tid=[main]; created={[main, t_fun@49-type-invariants.c:20:3-20:40#top]}}, thread:[main]] (conf. 100)  (exp: & tmp->field) (49-type-invariants.c:21:3-21:21)
  [1]
