  $ goblint --set ana.base.privatization write-tid --enable ana.int.interval --set ana.path_sens[+] mutex --set ana.race.graph-coloring greedy 78-write-tid.c
  [Success][Assert] Assertion "x >= 17" will succeed (78-write-tid.c:56:3-56:27)
  [Success][Assert] Assertion "x <= 42" will succeed (78-write-tid.c:59:3-59:27)
  [Success][Assert] Assertion "x <= 31" will succeed (78-write-tid.c:63:3-63:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 31
    dead: 0
    total lines: 31
  [Warning][Race] Memory location g (race with conf. 110): (78-write-tid.c:6:5-6:6)
    Safe subset 1:
      write with [lock:{c}, thread:[main, t2@78-write-tid.c:46:3-46:34]] (conf. 110)  (exp: & g) (78-write-tid.c:25:3-25:9)
      write with [mhp:{created={[main, t1@78-write-tid.c:45:3-45:34], [main, t2@78-write-tid.c:46:3-46:34]}}, lock:{c}, thread:[main]] (conf. 110)  (exp: & g) (78-write-tid.c:49:3-49:7)
    Safe subset 2:
      write with [lock:{a, b}, thread:[main, t1@78-write-tid.c:45:3-45:34]] (conf. 110)  (exp: & g) (78-write-tid.c:16:3-16:9)
      write with [lock:{b}, thread:[main, t1@78-write-tid.c:45:3-45:34]] (conf. 110)  (exp: & g) (78-write-tid.c:18:3-18:9)
    Safe subset 3:
      write with [lock:{a}, thread:[main, there_i_ruined_it@78-write-tid.c:65:3-65:49]] (conf. 110)  (exp: & g) (78-write-tid.c:33:3-33:9)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
