  $ goblint --enable warn.deterministic --enable allglobs 85-distribute-fields-2.c
  [Warning][Race] Memory location t.s.data (race with conf. 110): (85-distribute-fields-2.c:15:10-15:11)
    write with [mhp:{tid=[main, t_fun@85-distribute-fields-2.c:24:3-24:40#top]}, thread:[main, t_fun@85-distribute-fields-2.c:24:3-24:40#top]] (conf. 110)  (exp: & t.s.data) (85-distribute-fields-2.c:18:3-18:15)
    write with [mhp:{tid=[main]; created={[main, t_fun@85-distribute-fields-2.c:24:3-24:40#top]}}, thread:[main]] (conf. 110)  (exp: & t.s) (85-distribute-fields-2.c:26:3-26:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location t.s (safe): (85-distribute-fields-2.c:15:10-15:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@85-distribute-fields-2.c:24:3-24:40#top]}}, thread:[main]] (conf. 110)  (exp: & t.s) (85-distribute-fields-2.c:26:3-26:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
