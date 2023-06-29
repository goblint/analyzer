Disable info messages because race summary contains (safe) memory location count, which is different on Linux and OSX.

  $ goblint --enable warn.deterministic --disable warn.info --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" 16-type_rc.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:13:3-13:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:24:3-24:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:25:3-25:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:28:3-28:9)
  [Warning][Race] Memory location (struct s).datum (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@16-type_rc.c:27:3-27:37#top]}, thread:[main, t_fun@16-type_rc.c:27:3-27:37#top]] (conf. 100)  (exp: & s->datum) (16-type_rc.c:13:3-13:15)
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:12:12-12:24)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:23:3-23:14)

  $ goblint --enable warn.deterministic --disable warn.info --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 16-type_rc.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:13:3-13:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:24:3-24:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:25:3-25:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct s).datum (safe):
    write with [mhp:{tid=[main, t_fun@16-type_rc.c:27:3-27:37#top]}, thread:[main, t_fun@16-type_rc.c:27:3-27:37#top]] (conf. 100)  (exp: & s->datum) (16-type_rc.c:13:3-13:15)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:12:12-12:24)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:23:3-23:14)
