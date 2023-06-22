  $ goblint --enable allglobs 86-distribute-fields-3.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Success][Race] Memory location t@86-distribute-fields-3.c:15:10-15:11 (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@86-distribute-fields-3.c:24:3-24:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26:3-26:9)
  [Success][Race] Memory location t.s@86-distribute-fields-3.c:15:10-15:11 (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@86-distribute-fields-3.c:24:3-24:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26:3-26:9)
  [Warning][Race] Memory location t.s.data@86-distribute-fields-3.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@86-distribute-fields-3.c:24:3-24:40#top]}, thread:[main, t_fun@86-distribute-fields-3.c:24:3-24:40#top]] (conf. 110)  (exp: & t.s.data) (86-distribute-fields-3.c:18:3-18:15)
    write with [mhp:{tid=[main]; created={[main, t_fun@86-distribute-fields-3.c:24:3-24:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26:3-26:9)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3

TODO: fix memory location numbers
