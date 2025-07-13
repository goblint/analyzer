  $ goblint --enable warn.deterministic --enable allglobs 90-distribute-fields-type-1.c 2>&1 | tee default-output.txt
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (90-distribute-fields-type-1.c:31.3-31.20)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (90-distribute-fields-type-1.c:39.3-39.17)
  [Warning][Race] Memory location (struct T).s.field (race with conf. 100):
    write with thread:[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:31.3-31.20)
    write with [mhp:{created={[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:39.3-39.17)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
  [Success][Race] Memory location (struct S).field (safe):
    write with thread:[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:31.3-31.20)
  [Success][Race] Memory location (struct T).s (safe):
    write with [mhp:{created={[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:39.3-39.17)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Write to unknown address: privatization is unsound. (90-distribute-fields-type-1.c:31.3-31.20)
  [Info][Unsound] Write to unknown address: privatization is unsound. (90-distribute-fields-type-1.c:39.3-39.17)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (90-distribute-fields-type-1.c:31.3-31.20)
  [Info][Imprecise] Invalidating expressions: & tmp (90-distribute-fields-type-1.c:31.3-31.20)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (90-distribute-fields-type-1.c:39.3-39.17)
  [Info][Imprecise] Invalidating expressions: & tmp (90-distribute-fields-type-1.c:39.3-39.17)
  [Error][Imprecise][Unsound] Function definition missing for getS (90-distribute-fields-type-1.c:31.3-31.20)
  [Error][Imprecise][Unsound] Function definition missing for getT (90-distribute-fields-type-1.c:39.3-39.17)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --enable allglobs --enable dbg.full-output 90-distribute-fields-type-1.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  4,5c4,5
  <   write with thread:[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:31.3-31.20)
  <   write with [mhp:{created={[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:39.3-39.17)
  ---
  >   write with [mhp:{tid=[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40#⊤]}, thread:[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40#⊤]] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:31.3-31.20)
  >   write with [mhp:{tid=[main]; created={[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40#⊤]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:39.3-39.17)
  12c12
  <   write with thread:[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:31.3-31.20)
  ---
  >   write with [mhp:{tid=[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40#⊤]}, thread:[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40#⊤]] (conf. 100)  (exp: & tmp->field) (90-distribute-fields-type-1.c:31.3-31.20)
  14c14
  <   write with [mhp:{created={[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:39.3-39.17)
  ---
  >   write with [mhp:{tid=[main]; created={[main, t_fun@90-distribute-fields-type-1.c:37.3-37.40#⊤]}}, thread:[main]] (conf. 100)  (exp: & tmp->s) (90-distribute-fields-type-1.c:39.3-39.17)
  [1]
