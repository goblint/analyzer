  $ goblint --enable warn.deterministic --enable allglobs 87-distribute-fields-4.c 2>&1 | tee default-output.txt
  [Warning][Race] Memory location s (race with conf. 110): (87-distribute-fields-4.c:9:10-9:11)
    write with thread:[main, t_fun@87-distribute-fields-4.c:19:3-19:40] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:13:3-13:9)
    write with [mhp:{created={[main, t_fun@87-distribute-fields-4.c:19:3-19:40]}}, thread:[main]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:21:3-21:9)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8

  $ goblint --enable warn.deterministic --enable allglobs --enable dbg.full-output 87-distribute-fields-4.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  2,3c2,3
  <   write with thread:[main, t_fun@87-distribute-fields-4.c:19:3-19:40] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:13:3-13:9)
  <   write with [mhp:{created={[main, t_fun@87-distribute-fields-4.c:19:3-19:40]}}, thread:[main]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:21:3-21:9)
  ---
  >   write with [mhp:{tid=[main, t_fun@87-distribute-fields-4.c:19:3-19:40#⊤]}, thread:[main, t_fun@87-distribute-fields-4.c:19:3-19:40#⊤]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:13:3-13:9)
  >   write with [mhp:{tid=[main]; created={[main, t_fun@87-distribute-fields-4.c:19:3-19:40#⊤]}}, thread:[main]] (conf. 110)  (exp: & s) (87-distribute-fields-4.c:21:3-21:9)
  [1]
