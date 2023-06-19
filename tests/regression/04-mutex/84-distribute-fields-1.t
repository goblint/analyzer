  $ goblint --enable allglobs 84-distribute-fields-1.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Success][Race] Memory location s@84-distribute-fields-1.c:9:10-9:11 (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@84-distribute-fields-1.c:18:3-18:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (84-distribute-fields-1.c:20:3-20:9)
  [Warning][Race] Memory location s.data@84-distribute-fields-1.c:9:10-9:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@84-distribute-fields-1.c:18:3-18:40#top]}, thread:[main, t_fun@84-distribute-fields-1.c:18:3-18:40#top]] (conf. 110)  (exp: & s.data) (84-distribute-fields-1.c:12:3-12:13)
    write with [mhp:{tid=[main]; created={[main, t_fun@84-distribute-fields-1.c:18:3-18:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (84-distribute-fields-1.c:20:3-20:9)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
