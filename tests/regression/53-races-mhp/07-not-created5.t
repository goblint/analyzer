  $ goblint --set ana.activated[+] mhp --disable ana.thread.include-node --set ana.race.graph-coloring greedy --enable warn.deterministic 07-not-created5.c
  [Warning][Race] Memory location g (race with conf. 110): (07-not-created5.c:4:5-4:6)
    Self-races:
      write with [] (conf. 110)  (exp: & *gp) (07-not-created5.c:9:6-9:12)
    Safe subset 1:
      write with thread:[main, b, a] (conf. 110)  (exp: & *gp) (07-not-created5.c:9:6-9:12)
      read with thread:[main, b, a] (conf. 110)  (exp: & *gp) (07-not-created5.c:9:6-9:12)
    Safe subset 2:
      read with [] (conf. 110)  (exp: & *gp) (07-not-created5.c:9:6-9:12)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
