Disable info messages because race summary contains (safe) memory location count, which is different on Linux and OSX.

  $ goblint --enable warn.deterministic --disable warn.info --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" 21-mult_accs_rc.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:14:3-14:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:17:3-17:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:28:3-28:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:29:3-29:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:34:3-34:9)
  [Warning][Race] Memory location (struct s).data (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]}, thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
    write with [symblock:{p-lock:*.mutex}, mhp:{tid=[main]; created={[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (21-mult_accs_rc.c:34:3-34:9)
  [Warning][Unknown] unlocking mutex (NULL) which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:13:3-13:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:15:3-15:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:27:3-27:14)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --disable warn.info --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 21-mult_accs_rc.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:14:3-14:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:17:3-17:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:28:3-28:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:29:3-29:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:34:3-34:9)
  [Success][Race] Memory location (struct s).data (safe):
    write with [mhp:{tid=[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]}, thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Unknown] unlocking mutex (NULL) which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:13:3-13:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:15:3-15:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:27:3-27:14)
  [Error][Imprecise][Unsound] Function definition missing
