  $ goblint --set ana.activated[+] deadlock --enable ana.sv-comp.functions --set ana.race.graph-coloring greedy --enable warn.deterministic 27-self_deadlock.c
  [Error][Behavior > Undefined > DoubleLocking] Acquiring a non-recursive mutex that is already held (27-self_deadlock.c:14:5-14:32)
  [Error][Behavior > Undefined > DoubleLocking] Acquiring a non-recursive mutex that is already held (27-self_deadlock.c:24:5-24:32)
  [Warning][Race] Memory location g1 (race with conf. 110): (27-self_deadlock.c:7:5-7:7)
    Safe subset 1:
      write with lock:{mutex1} (conf. 110)  (exp: & g1) (27-self_deadlock.c:15:3-15:14)
      write with thread:[main, t1@27-self_deadlock.c:35:5-35:40], lock:{mutex1} (conf. 110)  (exp: & g1) (27-self_deadlock.c:15:3-15:14)
    Safe subset 2:
      read with lock:{mutex2} (conf. 110)  (exp: & g1) (27-self_deadlock.c:25:3-25:14)
      read with thread:[main, t2@27-self_deadlock.c:36:5-36:40], lock:{mutex2} (conf. 110)  (exp: & g1) (27-self_deadlock.c:25:3-25:14)
      read with thread:[main], mhp:{created={[main, t1@27-self_deadlock.c:35:5-35:40], [main, t2@27-self_deadlock.c:36:5-36:40], [main, {t1@27-self_deadlock.c:35:5-35:40}], [main, {t2@27-self_deadlock.c:36:5-36:40}]}} (conf. 110)  (exp: & g1) (27-self_deadlock.c:39:5-39:49)
  [Warning][Race] Memory location g2 (race with conf. 110): (27-self_deadlock.c:7:9-7:11)
    Safe subset 1:
      write with lock:{mutex2} (conf. 110)  (exp: & g2) (27-self_deadlock.c:25:3-25:14)
      write with thread:[main, t2@27-self_deadlock.c:36:5-36:40], lock:{mutex2} (conf. 110)  (exp: & g2) (27-self_deadlock.c:25:3-25:14)
    Safe subset 2:
      read with lock:{mutex1} (conf. 110)  (exp: & g2) (27-self_deadlock.c:15:3-15:14)
      read with thread:[main, t1@27-self_deadlock.c:35:5-35:40], lock:{mutex1} (conf. 110)  (exp: & g2) (27-self_deadlock.c:15:3-15:14)
      read with thread:[main], mhp:{created={[main, t1@27-self_deadlock.c:35:5-35:40], [main, t2@27-self_deadlock.c:36:5-36:40], [main, {t1@27-self_deadlock.c:35:5-35:40}], [main, {t2@27-self_deadlock.c:36:5-36:40}]}} (conf. 110)  (exp: & g2) (27-self_deadlock.c:39:5-39:49)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex1 with  (27-self_deadlock.c:12:3-12:30)
    lock after: mutex1 with lock:{mutex1} (27-self_deadlock.c:14:5-14:32)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex1 with thread:[main, t1@27-self_deadlock.c:35:5-35:40] (27-self_deadlock.c:12:3-12:30)
    lock after: mutex1 with thread:[main, t1@27-self_deadlock.c:35:5-35:40], lock:{mutex1} (27-self_deadlock.c:14:5-14:32)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex2 with  (27-self_deadlock.c:22:3-22:30)
    lock after: mutex2 with lock:{mutex2} (27-self_deadlock.c:24:5-24:32)
  [Warning][Deadlock] Locking order cycle:
    lock before: mutex2 with thread:[main, t2@27-self_deadlock.c:36:5-36:40] (27-self_deadlock.c:22:3-22:30)
    lock after: mutex2 with thread:[main, t2@27-self_deadlock.c:36:5-36:40], lock:{mutex2} (27-self_deadlock.c:24:5-24:32)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 25
    dead: 0
    total lines: 25
  [Warning][Unknown] unlocking mutex (mutex1) which may not be held (27-self_deadlock.c:17:3-17:32)
  [Warning][Unknown] unlocking mutex (mutex2) which may not be held (27-self_deadlock.c:27:3-27:32)
