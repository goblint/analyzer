  $ goblint --set ana.activated[-] thread --set ana.race.graph-coloring greedy 02-threadid_history_nonunique.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9
  [Warning][Race] Memory location myglobal (race with conf. 110): (02-threadid_history_nonunique.c:6:5-6:13)
    Self-races:
      write with [] (conf. 110)  (exp: & myglobal) (02-threadid_history_nonunique.c:9:3-9:14)
    Safe subset 1:
      write with thread:[main, t_fun@02-threadid_history_nonunique.c:17:5-17:45] (conf. 110)  (exp: & myglobal) (02-threadid_history_nonunique.c:9:3-9:14)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
