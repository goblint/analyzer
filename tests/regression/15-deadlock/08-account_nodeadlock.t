  $ goblint --set ana.activated[+] deadlock --enable ana.sv-comp.functions --set ana.race.graph-coloring greedy --enable warn.deterministic 08-account_nodeadlock.c
  [Warning][Race] Memory location A.balance (race with conf. 110): (08-account_nodeadlock.c:13:14-13:15)
    Safe subset 1:
      read with thread:[main], mhp:{created={[main, t1@08-account_nodeadlock.c:41:5-41:40], [main, t2@08-account_nodeadlock.c:42:5-42:40], [main, {t1@08-account_nodeadlock.c:41:5-41:40}], [main, {t2@08-account_nodeadlock.c:42:5-42:40}]}} (conf. 110)  (exp: & A.balance) (08-account_nodeadlock.c:45:5-45:61)
    Safe subset 2:
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:19:3-19:24)
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:19:3-19:24)
      write with thread:[main, t1@08-account_nodeadlock.c:41:5-41:40], lock:{A.mutex, B.mutex} (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:19:3-19:24)
      write with thread:[main, t2@08-account_nodeadlock.c:42:5-42:40], lock:{A.mutex, B.mutex} (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:19:3-19:24)
  [Warning][Race] Memory location B.balance (race with conf. 110): (08-account_nodeadlock.c:13:17-13:18)
    Safe subset 1:
      read with thread:[main], mhp:{created={[main, t1@08-account_nodeadlock.c:41:5-41:40], [main, t2@08-account_nodeadlock.c:42:5-42:40], [main, {t1@08-account_nodeadlock.c:41:5-41:40}], [main, {t2@08-account_nodeadlock.c:42:5-42:40}]}} (conf. 110)  (exp: & B.balance) (08-account_nodeadlock.c:45:5-45:61)
    Safe subset 2:
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:18:3-18:24)
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:18:3-18:24)
      write with thread:[main, t1@08-account_nodeadlock.c:41:5-41:40], lock:{A.mutex, B.mutex} (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:18:3-18:24)
      write with thread:[main, t2@08-account_nodeadlock.c:42:5-42:40], lock:{A.mutex, B.mutex} (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:18:3-18:24)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 25
    dead: 0
    total lines: 25
  [Info][Assumption] Mutexes are non-recursive by default (08-account_nodeadlock.c:37:3-37:36)
  [Info][Assumption] Mutexes are non-recursive by default (08-account_nodeadlock.c:38:3-38:36)
  [Info][Assumption] Mutexes are non-recursive by default
