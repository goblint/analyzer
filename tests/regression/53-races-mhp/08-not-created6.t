  $ goblint --set ana.activated[+] mhp --disable ana.thread.include-node --set ana.race.graph-coloring greedy --enable warn.deterministic 08-not-created6.c
  [Warning][Race] Memory location g (race with conf. 110): (08-not-created6.c:4:5-4:6)
    Self-races:
      write with [] (conf. 110)  (exp: & *gp) (08-not-created6.c:13:6-13:12)
    Safe subset 1:
      write with thread:[main, a, c] (conf. 110)  (exp: & *gp) (08-not-created6.c:13:6-13:12)
      read with thread:[main, a, c] (conf. 110)  (exp: & *gp) (08-not-created6.c:13:6-13:12)
    Safe subset 2:
      read with [] (conf. 110)  (exp: & *gp) (08-not-created6.c:13:6-13:12)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
