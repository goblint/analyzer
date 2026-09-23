  $ goblint --set ana.activated[+] deadlock --set ana.race.graph-coloring greedy 27-self_deadlock.c
  [Error][Behavior > Undefined > DoubleLocking] Acquiring a non-recursive mutex that is already held (27-self_deadlock.c:11:3-11:30)
  [Warning][Unknown] unlocking mutex (mutex1) which may not be held (27-self_deadlock.c:14:3-14:32)
  [Error][Behavior > Undefined > DoubleLocking] Acquiring a non-recursive mutex that is already held (27-self_deadlock.c:20:3-20:30)
  [Warning][Unknown] unlocking mutex (mutex2) which may not be held (27-self_deadlock.c:23:3-23:32)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 22
    dead: 0
    total lines: 22
  [Warning][Race] Memory location g1 (race with conf. 110): (27-self_deadlock.c:5:5-5:7)
    Safe subset 1:
      write with lock:{mutex1} (conf. 110)  (exp: & g1) (27-self_deadlock.c:12:3-12:14)
      write with [lock:{mutex1}, thread:[main, t1@27-self_deadlock.c:31:5-31:40]] (conf. 110)  (exp: & g1) (27-self_deadlock.c:12:3-12:14)
    Safe subset 2:
      read with lock:{mutex2} (conf. 110)  (exp: & g1) (27-self_deadlock.c:21:3-21:14)
      read with [lock:{mutex2}, thread:[main, t2@27-self_deadlock.c:32:5-32:40]] (conf. 110)  (exp: & g1) (27-self_deadlock.c:21:3-21:14)
      read with [mhp:{created={[main, t1@27-self_deadlock.c:31:5-31:40], [main, t2@27-self_deadlock.c:32:5-32:40], [main, {t1@27-self_deadlock.c:31:5-31:40}], [main, {t2@27-self_deadlock.c:32:5-32:40}]}}, thread:[main]] (conf. 110)  (exp: & g1) (27-self_deadlock.c:35:5-35:49)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex2 with [] (27-self_deadlock.c:19:3-19:30)
    lock after: mutex2 with lock:{mutex2} (27-self_deadlock.c:20:3-20:30)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex2 with thread:[main, t2@27-self_deadlock.c:32:5-32:40] (27-self_deadlock.c:19:3-19:30)
    lock after: mutex2 with [lock:{mutex2}, thread:[main, t2@27-self_deadlock.c:32:5-32:40]] (27-self_deadlock.c:20:3-20:30)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex1 with [] (27-self_deadlock.c:10:3-10:30)
    lock after: mutex1 with lock:{mutex1} (27-self_deadlock.c:11:3-11:30)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex1 with thread:[main, t1@27-self_deadlock.c:31:5-31:40] (27-self_deadlock.c:10:3-10:30)
    lock after: mutex1 with [lock:{mutex1}, thread:[main, t1@27-self_deadlock.c:31:5-31:40]] (27-self_deadlock.c:11:3-11:30)
  [Warning][Race] Memory location g2 (race with conf. 110): (27-self_deadlock.c:5:9-5:11)
    Safe subset 1:
      write with lock:{mutex2} (conf. 110)  (exp: & g2) (27-self_deadlock.c:21:3-21:14)
      write with [lock:{mutex2}, thread:[main, t2@27-self_deadlock.c:32:5-32:40]] (conf. 110)  (exp: & g2) (27-self_deadlock.c:21:3-21:14)
    Safe subset 2:
      read with lock:{mutex1} (conf. 110)  (exp: & g2) (27-self_deadlock.c:12:3-12:14)
      read with [lock:{mutex1}, thread:[main, t1@27-self_deadlock.c:31:5-31:40]] (conf. 110)  (exp: & g2) (27-self_deadlock.c:12:3-12:14)
      read with [mhp:{created={[main, t1@27-self_deadlock.c:31:5-31:40], [main, t2@27-self_deadlock.c:32:5-32:40], [main, {t1@27-self_deadlock.c:31:5-31:40}], [main, {t2@27-self_deadlock.c:32:5-32:40}]}}, thread:[main]] (conf. 110)  (exp: & g2) (27-self_deadlock.c:35:5-35:49)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2
