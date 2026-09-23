  $ goblint --disable ana.thread.context.create-edges --set ana.race.graph-coloring greedy 06-nc-deep.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 48
    dead: 0
    total lines: 48
  [Warning][Race] Memory location glob_create (race with conf. 110): (06-nc-deep.c:6:5-6:16)
    Self-races:
      write with [] (conf. 110)  (exp: & glob_create) (06-nc-deep.c:16:3-16:17)
    Safe subset 1:
      write with thread:[main, t_create@06-nc-deep.c:48:3-48:43] (conf. 110)  (exp: & glob_create) (06-nc-deep.c:16:3-16:17)
    Safe subset 2:
      write with thread:[main, t_create@06-nc-deep.c:72:3-72:43] (conf. 110)  (exp: & glob_create) (06-nc-deep.c:16:3-16:17)
      write with [mhp:{created={[main, t_INIT@06-nc-deep.c:54:3-54:41], [main, t_noCreate@06-nc-deep.c:62:3-62:45], [main, t_create@06-nc-deep.c:48:3-48:43]}}, thread:[main]] (conf. 110)  (exp: & glob_create) (06-nc-deep.c:69:3-69:18)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
