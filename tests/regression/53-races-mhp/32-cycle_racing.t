  $ goblint --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset --set ana.race.graph-coloring greedy --enable warn.deterministic 32-cycle_racing.c
  [Warning][Race] Memory location global (race with conf. 110): (32-cycle_racing.c:7:5-7:15)
    Self-races:
      write with  (conf. 110)  (exp: & global) (32-cycle_racing.c:29:3-29:11)
    Safe subset 1:
      write with thread:[main, t1@32-cycle_racing.c:36:3-36:38], lock:{mutex} (conf. 110)  (exp: & global) (32-cycle_racing.c:13:3-13:11)
      read with thread:[main, t1@32-cycle_racing.c:36:3-36:38], lock:{mutex} (conf. 110)  (exp: & global) (32-cycle_racing.c:13:3-13:11)
    Safe subset 2:
      write with thread:[main, tc1@32-cycle_racing.c:37:3-37:40, tc2@32-cycle_racing.c:31:3-31:40, tc1@32-cycle_racing.c:23:3-23:40] (conf. 110)  (exp: & global) (32-cycle_racing.c:29:3-29:11)
      write with thread:[main, tc1@32-cycle_racing.c:37:3-37:40] (conf. 110)  (exp: & global) (32-cycle_racing.c:29:3-29:11)
      read with thread:[main, tc1@32-cycle_racing.c:37:3-37:40, tc2@32-cycle_racing.c:31:3-31:40, tc1@32-cycle_racing.c:23:3-23:40] (conf. 110)  (exp: & global) (32-cycle_racing.c:29:3-29:11)
      read with thread:[main, tc1@32-cycle_racing.c:37:3-37:40] (conf. 110)  (exp: & global) (32-cycle_racing.c:29:3-29:11)
    Safe subset 3:
      read with  (conf. 110)  (exp: & global) (32-cycle_racing.c:29:3-29:11)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total lines: 18
