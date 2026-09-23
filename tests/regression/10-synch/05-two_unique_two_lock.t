  $ goblint --set ana.activated[+] thread --set ana.race.graph-coloring greedy 05-two_unique_two_lock.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Warning][Race] Memory location myglobal (race with conf. 110): (05-two_unique_two_lock.c:5:5-5:13)
    Safe subset 1:
      write with [lock:{B}, thread:[main, f1@05-two_unique_two_lock.c:31:3-31:37]] (conf. 110)  (exp: & myglobal) (05-two_unique_two_lock.c:14:3-14:13)
    Safe subset 2:
      write with [lock:{A}, thread:[main, f2@05-two_unique_two_lock.c:32:3-32:37]] (conf. 110)  (exp: & myglobal) (05-two_unique_two_lock.c:21:3-21:13)
  [Warning][Race] Memory location myglobal (race with conf. 110): (05-two_unique_two_lock.c:5:5-5:13)
    Safe subset 1:
      write with [lock:{A}, thread:[main, f1@05-two_unique_two_lock.c:31:3-31:37]] (conf. 110)  (exp: & myglobal) (05-two_unique_two_lock.c:11:3-11:13)
    Safe subset 2:
      write with [lock:{B}, thread:[main, f2@05-two_unique_two_lock.c:32:3-32:37]] (conf. 110)  (exp: & myglobal) (05-two_unique_two_lock.c:24:3-24:13)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
