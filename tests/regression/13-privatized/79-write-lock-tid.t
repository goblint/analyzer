  $ goblint --set ana.base.privatization write+lock-tid --enable ana.int.interval --set ana.path_sens[+] mutex --set ana.race.graph-coloring greedy --enable warn.deterministic 79-write-lock-tid.c
  [Success][Assert] Assertion "x >= 17" will succeed (79-write-lock-tid.c:68:3-68:27)
  [Success][Assert] Assertion "x <= 42" will succeed (79-write-lock-tid.c:71:3-71:27)
  [Success][Assert] Assertion "x <= 31" will succeed (79-write-lock-tid.c:75:3-75:27)
  [Success][Assert] Assertion "xx <= 17" will succeed (79-write-lock-tid.c:85:3-85:28)
  [Warning][Race] Memory location g (race with conf. 110): (79-write-lock-tid.c:6:5-6:6)
    Safe subset 1:
      write with [lock:{a, b}, thread:[main, t1@79-write-lock-tid.c:57:3-57:34]] (conf. 110)  (exp: & g) (79-write-lock-tid.c:20:3-20:9)
      write with [lock:{b}, thread:[main, t1@79-write-lock-tid.c:57:3-57:34]] (conf. 110)  (exp: & g) (79-write-lock-tid.c:22:3-22:9)
    Safe subset 2:
      write with [lock:{c}, thread:[main, t2@79-write-lock-tid.c:58:3-58:34]] (conf. 110)  (exp: & g) (79-write-lock-tid.c:36:3-36:9)
      write with [mhp:{created={[main, t1@79-write-lock-tid.c:57:3-57:34], [main, t2@79-write-lock-tid.c:58:3-58:34]}}, lock:{c}, thread:[main]] (conf. 110)  (exp: & g) (79-write-lock-tid.c:61:3-61:7)
    Safe subset 3:
      write with [lock:{a}, thread:[main, there_i_ruined_it@79-write-lock-tid.c:89:3-89:49]] (conf. 110)  (exp: & g) (79-write-lock-tid.c:44:3-44:9)
  [Warning][Race] Memory location xg (race with conf. 110): (79-write-lock-tid.c:11:5-11:7)
    Safe subset 1:
      write with [lock:{xd}, thread:[main, t1@79-write-lock-tid.c:57:3-57:34]] (conf. 110)  (exp: & xg) (79-write-lock-tid.c:29:3-29:10)
    Safe subset 2:
      read with [mhp:{created={[main, t1@79-write-lock-tid.c:57:3-57:34], [main, t2@79-write-lock-tid.c:58:3-58:34]}}, lock:{a, b, c, xa}, thread:[main]] (conf. 110)  (exp: & xg) (79-write-lock-tid.c:82:3-82:10)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 42
    dead: 0
    total lines: 42
