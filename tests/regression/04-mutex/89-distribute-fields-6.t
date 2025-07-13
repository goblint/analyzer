  $ goblint --enable warn.deterministic --enable allglobs 89-distribute-fields-6.c 2>&1 | tee default-output.txt
  [Warning][Race] Memory location t (race with conf. 110): (89-distribute-fields-6.c:15.10-15.11)
    write with thread:[main, t_fun@89-distribute-fields-6.c:25.3-25.40] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19.3-19.9)
    write with [mhp:{created={[main, t_fun@89-distribute-fields-6.c:25.3-25.40]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27.3-27.9)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8

  $ goblint --enable warn.deterministic --enable allglobs --enable dbg.full-output 89-distribute-fields-6.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  2,3c2,3
  <   write with thread:[main, t_fun@89-distribute-fields-6.c:25.3-25.40] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19.3-19.9)
  <   write with [mhp:{created={[main, t_fun@89-distribute-fields-6.c:25.3-25.40]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27.3-27.9)
  ---
  >   write with [mhp:{tid=[main, t_fun@89-distribute-fields-6.c:25.3-25.40#⊤]}, thread:[main, t_fun@89-distribute-fields-6.c:25.3-25.40#⊤]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:19.3-19.9)
  >   write with [mhp:{tid=[main]; created={[main, t_fun@89-distribute-fields-6.c:25.3-25.40#⊤]}}, thread:[main]] (conf. 110)  (exp: & t) (89-distribute-fields-6.c:27.3-27.9)
  [1]
