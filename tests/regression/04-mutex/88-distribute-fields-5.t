  $ goblint --enable allglobs 88-distribute-fields-5.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Warning][Race] Memory location t.s.data@88-distribute-fields-5.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]}, thread:[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]] (conf. 110)  (exp: & t.s) (88-distribute-fields-5.c:19:3-19:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t.s) (88-distribute-fields-5.c:27:3-27:11)
  [Warning][Race] Memory location t.s.data2@88-distribute-fields-5.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]}, thread:[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]] (conf. 110)  (exp: & t.s) (88-distribute-fields-5.c:19:3-19:11)
    write with [mhp:{tid=[main]; created={[main, t_fun@88-distribute-fields-5.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t.s) (88-distribute-fields-5.c:27:3-27:11)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 2
    total memory locations: 4

TODO: fix memory location counts