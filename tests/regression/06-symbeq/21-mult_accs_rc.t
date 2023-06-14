  $ goblint --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" 21-mult_accs_rc.c
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:27:3-27:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (21-mult_accs_rc.c:27:3-27:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (21-mult_accs_rc.c:27:3-27:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (21-mult_accs_rc.c:27:3-27:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (21-mult_accs_rc.c:27:3-27:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:28:3-28:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:29:3-29:15)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:13:3-13:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (21-mult_accs_rc.c:13:3-13:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (21-mult_accs_rc.c:13:3-13:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (21-mult_accs_rc.c:13:3-13:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (21-mult_accs_rc.c:13:3-13:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:14:3-14:32)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:15:3-15:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (21-mult_accs_rc.c:15:3-15:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (21-mult_accs_rc.c:15:3-15:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (21-mult_accs_rc.c:15:3-15:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (21-mult_accs_rc.c:15:3-15:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:16:3-16:14)
  [Info][Unsound] Write to unknown address: privatization is unsound. (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:17:3-17:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:34:3-34:9)
  [Info][Unsound] Write to unknown address: privatization is unsound. (21-mult_accs_rc.c:34:3-34:9)
  [Info][Unsound] Unknown mutex unlocked, base privatization unsound (21-mult_accs_rc.c:35:3-35:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16
  [Warning][Race] Memory location (struct s).data (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]}, thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
    write with [symblock:{p-lock:*.mutex}, mhp:{tid=[main]; created={[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (21-mult_accs_rc.c:34:3-34:9)
  [Info][Race] Memory locations race summary:
    safe: 14
    vulnerable: 0
    unsafe: 1
    total memory locations: 15

  $ goblint --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 21-mult_accs_rc.c
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:27:3-27:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (21-mult_accs_rc.c:27:3-27:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (21-mult_accs_rc.c:27:3-27:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (21-mult_accs_rc.c:27:3-27:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (21-mult_accs_rc.c:27:3-27:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:28:3-28:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:29:3-29:15)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:13:3-13:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (21-mult_accs_rc.c:13:3-13:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (21-mult_accs_rc.c:13:3-13:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (21-mult_accs_rc.c:13:3-13:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (21-mult_accs_rc.c:13:3-13:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:14:3-14:32)
  [Error][Imprecise][Unsound] Function definition missing for get_s (21-mult_accs_rc.c:15:3-15:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (21-mult_accs_rc.c:15:3-15:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (21-mult_accs_rc.c:15:3-15:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (21-mult_accs_rc.c:15:3-15:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (21-mult_accs_rc.c:15:3-15:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:16:3-16:14)
  [Info][Unsound] Write to unknown address: privatization is unsound. (21-mult_accs_rc.c:16:3-16:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:17:3-17:32)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (21-mult_accs_rc.c:34:3-34:9)
  [Info][Unsound] Write to unknown address: privatization is unsound. (21-mult_accs_rc.c:34:3-34:9)
  [Info][Unsound] Unknown mutex unlocked, base privatization unsound (21-mult_accs_rc.c:35:3-35:26)
  [Warning][Unknown] unlocking unknown mutex which may not be held (21-mult_accs_rc.c:35:3-35:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16
  [Success][Race] Memory location (struct s).data (safe):
    write with [mhp:{tid=[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]}, thread:[main, t_fun@21-mult_accs_rc.c:31:3-31:37#top]] (conf. 100)  (exp: & s->data) (21-mult_accs_rc.c:16:3-16:14)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
