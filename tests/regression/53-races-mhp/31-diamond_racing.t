  $ goblint --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset --set ana.race.graph-coloring greedy --enable warn.deterministic 31-diamond_racing.c
  [Warning][Race] Memory location global (race with conf. 110): (31-diamond_racing.c:4:5-4:15)
    Safe subset 1:
      write with [thread:[main, t2@31-diamond_racing.c:36:3-36:38, t4@31-diamond_racing.c:29:3-29:40]] (conf. 110)  (exp: & global) (31-diamond_racing.c:16:3-16:11)
      read with [thread:[main, t2@31-diamond_racing.c:36:3-36:38, t4@31-diamond_racing.c:29:3-29:40]] (conf. 110)  (exp: & global) (31-diamond_racing.c:16:3-16:11)
    Safe subset 2:
      write with [thread:[main, t1@31-diamond_racing.c:35:3-35:38], lock:{mutex}] (conf. 110)  (exp: & global) (31-diamond_racing.c:10:3-10:11)
      write with [thread:[main, t3@31-diamond_racing.c:37:3-37:38, t4@31-diamond_racing.c:22:3-22:40], creationLockset:{
                                                                                                                       [main, t3@31-diamond_racing.c:37:3-37:38] -> {mutex}
                                                                                                                       [main] -> {}
                                                                                                                     }] (conf. 110)  (exp: & global) (31-diamond_racing.c:16:3-16:11)
      read with [thread:[main, t1@31-diamond_racing.c:35:3-35:38], lock:{mutex}] (conf. 110)  (exp: & global) (31-diamond_racing.c:10:3-10:11)
      read with [thread:[main, t3@31-diamond_racing.c:37:3-37:38, t4@31-diamond_racing.c:22:3-22:40], creationLockset:{
                                                                                                                      [main, t3@31-diamond_racing.c:37:3-37:38] -> {mutex}
                                                                                                                      [main] -> {}
                                                                                                                    }] (conf. 110)  (exp: & global) (31-diamond_racing.c:16:3-16:11)
  [Info][Race] Memory locations race summary:
    safe: 5
    vulnerable: 0
    unsafe: 1
    total memory locations: 6
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 23
    dead: 0
    total lines: 23
