  $ goblint --enable warn.deterministic --enable allglobs 86-distribute-fields-3.c 2>&1 | tee default-output.txt
  [Warning][Race] Memory location t.s.data (race with conf. 110): (86-distribute-fields-3.c:15.10-15.11)
    write with thread:[main, t_fun@86-distribute-fields-3.c:24.3-24.40] (conf. 110)  (exp: & t.s.data) (86-distribute-fields-3.c:18.3-18.15)
    write with [mhp:{created={[main, t_fun@86-distribute-fields-3.c:24.3-24.40]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26.3-26.9)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
  [Success][Race] Memory location t (safe): (86-distribute-fields-3.c:15.10-15.11)
    write with [mhp:{created={[main, t_fun@86-distribute-fields-3.c:24.3-24.40]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26.3-26.9)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8

  $ goblint --enable warn.deterministic --enable allglobs --enable dbg.full-output 86-distribute-fields-3.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  2,3c2,3
  <   write with thread:[main, t_fun@86-distribute-fields-3.c:24.3-24.40] (conf. 110)  (exp: & t.s.data) (86-distribute-fields-3.c:18.3-18.15)
  <   write with [mhp:{created={[main, t_fun@86-distribute-fields-3.c:24.3-24.40]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26.3-26.9)
  ---
  >   write with [mhp:{tid=[main, t_fun@86-distribute-fields-3.c:24.3-24.40#⊤]}, thread:[main, t_fun@86-distribute-fields-3.c:24.3-24.40#⊤]] (conf. 110)  (exp: & t.s.data) (86-distribute-fields-3.c:18.3-18.15)
  >   write with [mhp:{tid=[main]; created={[main, t_fun@86-distribute-fields-3.c:24.3-24.40#⊤]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26.3-26.9)
  10c10
  <   write with [mhp:{created={[main, t_fun@86-distribute-fields-3.c:24.3-24.40]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26.3-26.9)
  ---
  >   write with [mhp:{tid=[main]; created={[main, t_fun@86-distribute-fields-3.c:24.3-24.40#⊤]}}, thread:[main]] (conf. 110)  (exp: & t) (86-distribute-fields-3.c:26.3-26.9)
  [1]
