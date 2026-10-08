  $ goblint --enable warn.deterministic --enable allglobs 80-type-nested-fields-deep2.c 2>&1 | tee default-output.txt
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (80-type-nested-fields-deep2.c:36:3-36:22)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (80-type-nested-fields-deep2.c:43:3-43:24)
  [Warning][Race] Memory location (struct U).t.s.field (race with conf. 100):
    write with thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40] (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
    write with thread:[main], mhp:{created={[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40]}} (conf. 100)  (exp: & tmp->t.s.field) (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location (struct T).s.field (safe):
    write with thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40] (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Unsound] Unknown address in __goblint_initial_stderr has escaped. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown address in __goblint_initial_stdin has escaped. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown address in __goblint_initial_stdout has escaped. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown address in stderr has escaped. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown address in stdin has escaped. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown address in stdout has escaped. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Write to unknown address: privatization is unsound. (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Unsound] Unknown address in __goblint_initial_stderr has escaped. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Unknown address in __goblint_initial_stdin has escaped. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Unknown address in __goblint_initial_stdout has escaped. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Unknown address in stderr has escaped. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Unknown address in stdin has escaped. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Unknown address in stdout has escaped. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Unsound] Write to unknown address: privatization is unsound. (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Imprecise] Invalidating expressions: & stderr, & stdout, & stdin (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Imprecise] Invalidating expressions: & tmp (80-type-nested-fields-deep2.c:36:3-36:22)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Imprecise] Invalidating expressions: & stderr, & stdout, & stdin (80-type-nested-fields-deep2.c:43:3-43:24)
  [Info][Imprecise] Invalidating expressions: & tmp (80-type-nested-fields-deep2.c:43:3-43:24)
  [Error][Imprecise][Unsound] Function definition missing for getT (80-type-nested-fields-deep2.c:36:3-36:22)
  [Error][Imprecise][Unsound] Function definition missing for getU (80-type-nested-fields-deep2.c:43:3-43:24)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --enable allglobs --enable dbg.full-output 80-type-nested-fields-deep2.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  4,5c4,5
  <   write with thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40] (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
  <   write with thread:[main], mhp:{created={[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40]}} (conf. 100)  (exp: & tmp->t.s.field) (80-type-nested-fields-deep2.c:43:3-43:24)
  ---
  >   write with thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#⊤], mhp:{tid=[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#⊤]} (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
  >   write with thread:[main], mhp:{tid=[main]; created={[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#⊤]}} (conf. 100)  (exp: & tmp->t.s.field) (80-type-nested-fields-deep2.c:43:3-43:24)
  12c12
  <   write with thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40] (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
  ---
  >   write with thread:[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#⊤], mhp:{tid=[main, t_fun@80-type-nested-fields-deep2.c:42:3-42:40#⊤]} (conf. 100)  (exp: & tmp->s.field) (80-type-nested-fields-deep2.c:36:3-36:22)
  [1]
