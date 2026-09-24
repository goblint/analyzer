  $ goblint --enable warn.deterministic 01-simple_rc.c 2>&1 | tee default-output.txt
  [Warning][Race] Memory location myglobal (race with conf. 110): (01-simple_rc.c:4:5-4:13)
    write with thread:[main, t_fun@01-simple_rc.c:17:3-17:40], lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
    write with thread:[main], lock:{mutex2}, mhp:{created={[main, t_fun@01-simple_rc.c:17:3-17:40]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
    read with thread:[main, t_fun@01-simple_rc.c:17:3-17:40], lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
    read with thread:[main], lock:{mutex2}, mhp:{created={[main, t_fun@01-simple_rc.c:17:3-17:40]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12

  $ goblint --enable warn.deterministic --enable dbg.full-output 01-simple_rc.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  2,5c2,5
  <   write with thread:[main, t_fun@01-simple_rc.c:17:3-17:40], lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
  <   write with thread:[main], lock:{mutex2}, mhp:{created={[main, t_fun@01-simple_rc.c:17:3-17:40]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  <   read with thread:[main, t_fun@01-simple_rc.c:17:3-17:40], lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
  <   read with thread:[main], lock:{mutex2}, mhp:{created={[main, t_fun@01-simple_rc.c:17:3-17:40]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  ---
  >   write with thread:[main, t_fun@01-simple_rc.c:17:3-17:40#⊤], lock:{mutex1}, mhp:{tid=[main, t_fun@01-simple_rc.c:17:3-17:40#⊤]} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
  >   write with thread:[main], lock:{mutex2}, mhp:{tid=[main]; created={[main, t_fun@01-simple_rc.c:17:3-17:40#⊤]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  >   read with thread:[main, t_fun@01-simple_rc.c:17:3-17:40#⊤], lock:{mutex1}, mhp:{tid=[main, t_fun@01-simple_rc.c:17:3-17:40#⊤]} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
  >   read with thread:[main], lock:{mutex2}, mhp:{tid=[main]; created={[main, t_fun@01-simple_rc.c:17:3-17:40#⊤]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  [1]


Race coloring:

  $ goblint --set ana.race.graph-coloring greedy --enable warn.deterministic 01-simple_rc.c
  [Warning][Race] Memory location myglobal (race with conf. 110): (01-simple_rc.c:4:5-4:13)
    Safe subset 1:
      write with thread:[main, t_fun@01-simple_rc.c:17:3-17:40], lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
      read with thread:[main, t_fun@01-simple_rc.c:17:3-17:40], lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
    Safe subset 2:
      write with thread:[main], lock:{mutex2}, mhp:{created={[main, t_fun@01-simple_rc.c:17:3-17:40]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
      read with thread:[main], lock:{mutex2}, mhp:{created={[main, t_fun@01-simple_rc.c:17:3-17:40]}} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12

Without thread ID and MHP now:

  $ goblint --set ana.race.graph-coloring greedy --set ana.activated[-] threadid --set ana.activated[-] mhp --enable warn.deterministic 01-simple_rc.c
  [Warning][Race] Memory location myglobal (race with conf. 110): (01-simple_rc.c:4:5-4:13)
    Safe subset 1:
      write with lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
      read with lock:{mutex1} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:10:3-10:22)
    Safe subset 2:
      write with lock:{mutex2} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
      read with lock:{mutex2} (conf. 110)  (exp: & myglobal) (01-simple_rc.c:19:3-19:22)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
