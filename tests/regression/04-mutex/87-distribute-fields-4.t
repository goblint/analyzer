  $ goblint --enable allglobs 87-distribute-fields-4.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Warning][Race] Memory location s.data@87-distribute-fields-4.c:9:10-9:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@87-distribute-fields-4.c:19:3-19:40#top]}, thread:[main, t_fun@87-distribute-fields-4.c:19:3-19:40#top]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:13:3-13:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@87-distribute-fields-4.c:19:3-19:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:21:3-21:9)
  [Warning][Race] Memory location s.data2@87-distribute-fields-4.c:9:10-9:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@87-distribute-fields-4.c:19:3-19:40#top]}, thread:[main, t_fun@87-distribute-fields-4.c:19:3-19:40#top]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:13:3-13:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@87-distribute-fields-4.c:19:3-19:40#top]}}, thread:[main]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:21:3-21:9)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 2
    total memory locations: 3

TODO: fix memory location counts