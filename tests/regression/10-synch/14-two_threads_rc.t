  $ goblint --set ana.activated[+] thread --set ana.activated[+] threadJoins --set ana.race.graph-coloring greedy 14-two_threads_rc.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
  [Warning][Race] Memory location myglobal (race with conf. 110): (14-two_threads_rc.c:5:5-5:13)
    Safe subset 1:
      write with thread:[main, t_fun@14-two_threads_rc.c:15:3-15:40] (conf. 110)  (exp: & myglobal) (14-two_threads_rc.c:8:3-8:14)
    Safe subset 2:
      write with [mhp:{created={[main, t_fun@14-two_threads_rc.c:15:3-15:40], [main, t_fun@14-two_threads_rc.c:16:3-16:40]}}, thread:[main]] (conf. 110)  (exp: & myglobal) (14-two_threads_rc.c:17:3-17:15)
    Safe subset 3:
      write with thread:[main, t_fun@14-two_threads_rc.c:16:3-16:40] (conf. 110)  (exp: & myglobal) (14-two_threads_rc.c:8:3-8:14)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
