  $ goblint --enable allglobs 89-distribute-fields-6.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Warning][Race] Memory location t.data3@89-distribute-fields-6.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}, thread:[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19:3-19:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27:3-27:9)
  [Warning][Race] Memory location t.s.data@89-distribute-fields-6.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}, thread:[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19:3-19:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27:3-27:9)
  [Warning][Race] Memory location t.s.data2@89-distribute-fields-6.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}, thread:[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19:3-19:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27:3-27:9)
  [Warning][Race] Memory location t.s2.data@89-distribute-fields-6.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}, thread:[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19:3-19:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27:3-27:9)
  [Warning][Race] Memory location t.s2.data2@89-distribute-fields-6.c:15:10-15:11 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}, thread:[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19:3-19:9)
    write with [mhp:{tid=[main]; created={[main, t_fun@89-distribute-fields-6.c:25:3-25:40#top]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27:3-27:9)
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 5
    total memory locations: 8

TODO: fix memory location counts