Disable info messages because race summary contains (safe) memory location count, which is different on Linux and OSX.

  $ goblint --enable warn.deterministic --disable warn.info --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" 21-mult_accs_rc.c 2>&1 | tee default-output-1.txt
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:14:3-14:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:17:3-17:34)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:28:3-28:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:29:3-29:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:34:3-34:9)
  [Warning][Race] Memory location (struct s).data (race with conf. 100):
    write with thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
    write with [symblock:{p-lock:*.mutex}, mhp:{created={[main, t_fun@21-mult_accs_rc.c:31:3-31:37]}}, thread:[main]] (conf. 100)  (exp: & *d) (21-mult_accs_rc.c:34:3-34:9)
  [Warning][Unknown] locking NULL mutex (21-mult_accs_rc.c:14:3-14:32)
  [Warning][Unknown] unlocking NULL mutex (21-mult_accs_rc.c:17:3-17:34)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:17:3-17:34)
  [Warning][Unknown] locking NULL mutex (21-mult_accs_rc.c:33:3-33:24)
  [Warning][Unknown] unlocking NULL mutex (21-mult_accs_rc.c:35:3-35:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:13:3-13:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:15:3-15:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:27:3-27:14)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --disable warn.info --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 21-mult_accs_rc.c 2>&1 | tee default-output-2.txt
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:14:3-14:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:17:3-17:34)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:28:3-28:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:29:3-29:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:34:3-34:9)
  [Success][Race] Memory location (struct s).data (safe):
    write with thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Unknown] locking NULL mutex (21-mult_accs_rc.c:14:3-14:32)
  [Warning][Unknown] unlocking NULL mutex (21-mult_accs_rc.c:17:3-17:34)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:17:3-17:34)
  [Warning][Unknown] locking NULL mutex (21-mult_accs_rc.c:33:3-33:24)
  [Warning][Unknown] unlocking NULL mutex (21-mult_accs_rc.c:35:3-35:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:13:3-13:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:15:3-15:14)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:27:3-27:14)
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --enable warn.deterministic --disable warn.info --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable dbg.full-output 21-mult_accs_rc.c > full-output-1.txt 2>&1

  $ diff default-output-1.txt full-output-1.txt
  8,9c8,9
  <   write with thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  <   write with [symblock:{p-lock:*.mutex}, mhp:{created={[main, t_fun@21-mult_accs_rc.c:31:3-31:37]}}, thread:[main]] (conf. 100)  (exp: & *d) (21-mult_accs_rc.c:34:3-34:9)
  ---
  >   write with [mhp:{tid=[main, t_fun@21-mult_accs_rc.c:31:3-31:37#⊤]}, thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37#⊤]] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  >   write with [symblock:{p-lock:*.mutex}, mhp:{tid=[main]; created={[main, t_fun@21-mult_accs_rc.c:31:3-31:37#⊤]}}, thread:[main]] (conf. 100)  (exp: & *d) (21-mult_accs_rc.c:34:3-34:9)
  [1]

  $ goblint --enable warn.deterministic --disable warn.info --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs --enable dbg.full-output 21-mult_accs_rc.c > full-output-2.txt 2>&1

  $ diff default-output-2.txt full-output-2.txt
  8c8
  <   write with thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  ---
  >   write with [mhp:{tid=[main, t_fun@21-mult_accs_rc.c:31:3-31:37#⊤]}, thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37#⊤]] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  [1]
