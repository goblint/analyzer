  $ goblint --set ana.race.graph-coloring greedy 25-single_acc.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Race] Memory location x (race with conf. 110): (25-single_acc.c:3:5-3:6)
    Safe subset 1:
      write with thread:[main, t_fun@25-single_acc.c:13:3-13:41] (conf. 110)  (exp: & x) (25-single_acc.c:6:3-6:6)
      read with thread:[main, t_fun@25-single_acc.c:13:3-13:41] (conf. 110)  (exp: & x) (25-single_acc.c:6:3-6:6)
    Safe subset 2:
      write with thread:[main, t_fun@25-single_acc.c:14:3-14:41] (conf. 110)  (exp: & x) (25-single_acc.c:6:3-6:6)
      read with thread:[main, t_fun@25-single_acc.c:14:3-14:41] (conf. 110)  (exp: & x) (25-single_acc.c:6:3-6:6)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
