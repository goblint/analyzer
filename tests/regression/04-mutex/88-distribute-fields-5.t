  $ goblint --enable warn.deterministic --enable allglobs 88-distribute-fields-5.c
  [Warning][Race] Memory location t.s (race with conf. 110): (88-distribute-fields-5.c:15:10-15:11)
    write with [mhp:{tid=[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]}, thread:[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]] (conf. 110)  (exp: & t.s) (88-distribute-fields-5.c:19:3-19:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t.s) (88-distribute-fields-5.c:27:3-27:11)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
