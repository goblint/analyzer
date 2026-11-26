  $ goblint --set ana.activated[+] deadlock --enable warn.deterministic 28-self_must_deadlock.c
  [Error][Behavior > Undefined > DoubleLocking] Acquiring a non-recursive mutex that is already held (28-self_must_deadlock.c:12:3-12:30)
  [Error][Behavior > Undefined > DoubleLocking] Acquiring a non-recursive mutex that is already held (28-self_must_deadlock.c:21:3-21:30)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex1 with  (28-self_must_deadlock.c:11:3-11:30)
    lock after: mutex1 with lock:{mutex1} (28-self_must_deadlock.c:12:3-12:30)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex1 with thread:[main, t1@28-self_must_deadlock.c:32:5-32:40] (28-self_must_deadlock.c:11:3-11:30)
    lock after: mutex1 with thread:[main, t1@28-self_must_deadlock.c:32:5-32:40], lock:{mutex1} (28-self_must_deadlock.c:12:3-12:30)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex2 with  (28-self_must_deadlock.c:20:3-20:30)
    lock after: mutex2 with lock:{mutex2} (28-self_must_deadlock.c:21:3-21:30)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex2 with thread:[main, t2@28-self_must_deadlock.c:33:5-33:40] (28-self_must_deadlock.c:20:3-20:30)
    lock after: mutex2 with thread:[main, t2@28-self_must_deadlock.c:33:5-33:40], lock:{mutex2} (28-self_must_deadlock.c:21:3-21:30)
  [Warning][Deadcode] Function 't1' has dead code:
    on lines 13..16 (28-self_must_deadlock.c:13-16)
  [Warning][Deadcode] Function 't2' has dead code:
    on lines 22..25 (28-self_must_deadlock.c:22-25)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 8
    total lines: 22
