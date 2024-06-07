  $ goblint --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" 14-list_entry_rc.c 2>&1 | sed -r 's/sid:[0-9]+/sid:$SID/' | tee default-output.txt
  [Info][Imprecise] Invalidating expressions: & mutexattr (14-list_entry_rc.c:37:3-37:37)
  [Warning][Unknown] unlocking mutex ((alloc@sid:$SID@tid:[main])[1].mutex) which may not be held (14-list_entry_rc.c:28:3-28:34)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 23
    dead: 0
    total lines: 23
  [Warning][Race] Memory location (alloc@sid:$SID@tid:[main])[?].datum (race with conf. 110): (14-list_entry_rc.c:41:3-41:35)
    write with thread:[main, t_fun@14-list_entry_rc.c:45:3-45:40] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
    write with [mhp:{created={[main, t_fun@14-list_entry_rc.c:45:3-45:40]}}, thread:[main]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
    read with thread:[main, t_fun@14-list_entry_rc.c:45:3-45:40] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
    read with [mhp:{created={[main, t_fun@14-list_entry_rc.c:45:3-45:40]}}, thread:[main]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2

  $ goblint --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'" --set ana.activated[+] "'symb_locks'" --enable dbg.full-output 14-list_entry_rc.c 2>&1 | sed -r 's/sid:[0-9]+/sid:$SID/' > full-output.txt

  $ diff default-output.txt full-output.txt
  2c2
  < [Warning][Unknown] unlocking mutex ((alloc@sid:$SID@tid:[main])[1].mutex) which may not be held (14-list_entry_rc.c:28:3-28:34)
  ---
  > [Warning][Unknown] unlocking mutex ((alloc@sid:$SID@tid:[main](#top))[def_exc:1].mutex) which may not be held (14-list_entry_rc.c:28:3-28:34)
  7,11c7,11
  < [Warning][Race] Memory location (alloc@sid:$SID@tid:[main])[?].datum (race with conf. 110): (14-list_entry_rc.c:41:3-41:35)
  <   write with thread:[main, t_fun@14-list_entry_rc.c:45:3-45:40] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  <   write with [mhp:{created={[main, t_fun@14-list_entry_rc.c:45:3-45:40]}}, thread:[main]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  <   read with thread:[main, t_fun@14-list_entry_rc.c:45:3-45:40] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  <   read with [mhp:{created={[main, t_fun@14-list_entry_rc.c:45:3-45:40]}}, thread:[main]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  ---
  > [Warning][Race] Memory location (alloc@sid:$SID@tid:[main](#top))[?].datum (race with conf. 110): (14-list_entry_rc.c:41:3-41:35)
  >   write with [mhp:{tid=[main, t_fun@14-list_entry_rc.c:45:3-45:40#⊤]}, thread:[main, t_fun@14-list_entry_rc.c:45:3-45:40#⊤]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  >   write with [mhp:{tid=[main]; created={[main, t_fun@14-list_entry_rc.c:45:3-45:40#⊤]}}, thread:[main]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  >   read with [mhp:{tid=[main, t_fun@14-list_entry_rc.c:45:3-45:40#⊤]}, thread:[main, t_fun@14-list_entry_rc.c:45:3-45:40#⊤]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  >   read with [mhp:{tid=[main]; created={[main, t_fun@14-list_entry_rc.c:45:3-45:40#⊤]}}, thread:[main]] (conf. 110)  (exp: & s->datum) (14-list_entry_rc.c:27:3-27:13)
  [1]
