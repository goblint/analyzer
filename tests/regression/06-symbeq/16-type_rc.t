Disable info messages because race summary contains (safe) memory location count, which is different on Linux and OSX.

  $ goblint --enable warn.deterministic --disable warn.info --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" 16-type_rc.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:21:3-21:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:32:3-32:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:33:3-33:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:36:3-36:9)
  [Warning][Race] Memory location (struct s).datum (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@16-type_rc.c:35:3-35:37#top]}, thread:[main, t_fun@16-type_rc.c:35:3-35:37#top]] (conf. 100)  (exp: & s->datum) (16-type_rc.c:21:3-21:15)
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:35:3-35:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:36:3-36:9)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:20:12-20:24)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:31:3-31:14)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --disable warn.info --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 16-type_rc.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:21:3-21:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:32:3-32:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:33:3-33:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:36:3-36:9)
  [Success][Race] Memory location (struct s).datum (safe):
    write with [mhp:{tid=[main, t_fun@16-type_rc.c:35:3-35:37#top]}, thread:[main, t_fun@16-type_rc.c:35:3-35:37#top]] (conf. 100)  (exp: & s->datum) (16-type_rc.c:21:3-21:15)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:20:12-20:24)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:31:3-31:14)
  [Error][Imprecise][Unsound] Function definition missing
