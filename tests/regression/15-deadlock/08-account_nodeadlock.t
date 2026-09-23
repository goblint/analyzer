  $ goblint --set ana.activated[+] deadlock --set ana.race.graph-coloring greedy --enable warn.deterministic 08-account_nodeadlock.c
  [Warning][Race] Memory location A.balance (race with conf. 110): (08-account_nodeadlock.c:11:14-11:15)
    Safe subset 1:
      read with [mhp:{created={[main, t1@08-account_nodeadlock.c:39:5-39:40], [main, t2@08-account_nodeadlock.c:40:5-40:40], [main, {t1@08-account_nodeadlock.c:39:5-39:40}], [main, {t2@08-account_nodeadlock.c:40:5-40:40}]}}, thread:[main]] (conf. 110)  (exp: & A.balance) (08-account_nodeadlock.c:43:5-43:61)
    Safe subset 2:
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:17:3-17:24)
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:17:3-17:24)
      write with [lock:{A.mutex, B.mutex}, thread:[main, t1@08-account_nodeadlock.c:39:5-39:40]] (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:17:3-17:24)
      write with [lock:{A.mutex, B.mutex}, thread:[main, t2@08-account_nodeadlock.c:40:5-40:40]] (conf. 110)  (exp: & f->balance) (08-account_nodeadlock.c:17:3-17:24)
  [Warning][Race] Memory location B.balance (race with conf. 110): (08-account_nodeadlock.c:11:17-11:18)
    Safe subset 1:
      read with [mhp:{created={[main, t1@08-account_nodeadlock.c:39:5-39:40], [main, t2@08-account_nodeadlock.c:40:5-40:40], [main, {t1@08-account_nodeadlock.c:39:5-39:40}], [main, {t2@08-account_nodeadlock.c:40:5-40:40}]}}, thread:[main]] (conf. 110)  (exp: & B.balance) (08-account_nodeadlock.c:43:5-43:61)
    Safe subset 2:
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:16:3-16:24)
      write with lock:{A.mutex, B.mutex} (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:16:3-16:24)
      write with [lock:{A.mutex, B.mutex}, thread:[main, t1@08-account_nodeadlock.c:39:5-39:40]] (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:16:3-16:24)
      write with [lock:{A.mutex, B.mutex}, thread:[main, t2@08-account_nodeadlock.c:40:5-40:40]] (conf. 110)  (exp: & t->balance) (08-account_nodeadlock.c:16:3-16:24)
  [Warning][Race] Memory location rand (race with conf. 110): (/usr/include/stdlib.h:573:12-573:24)
    Self-races:
      call with [] (conf. 110)  (exp: rand) (08-account_nodeadlock.c:24:3-24:32)
      call with [] (conf. 110)  (exp: rand) (08-account_nodeadlock.c:29:3-29:35)
    Safe subset 1:
      call with thread:[main, t1@08-account_nodeadlock.c:39:5-39:40] (conf. 110)  (exp: rand) (08-account_nodeadlock.c:24:3-24:32)
    Safe subset 2:
      call with thread:[main, t2@08-account_nodeadlock.c:40:5-40:40] (conf. 110)  (exp: rand) (08-account_nodeadlock.c:29:3-29:35)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 3
    total memory locations: 3
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 24
    dead: 0
    total lines: 24
  [Info][Assumption] Mutexes are non-recursive by default (08-account_nodeadlock.c:35:3-35:36)
  [Info][Assumption] Mutexes are non-recursive by default (08-account_nodeadlock.c:36:3-36:36)
  [Info][Assumption] Mutexes are non-recursive by default
