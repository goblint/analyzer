  $ goblint --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 16-type_rc.c
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:23:3-23:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (16-type_rc.c:23:3-23:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (16-type_rc.c:23:3-23:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (16-type_rc.c:23:3-23:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (16-type_rc.c:23:3-23:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:24:3-24:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:25:3-25:16)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:12:12-12:24)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (16-type_rc.c:12:12-12:24)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (16-type_rc.c:12:12-12:24)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (16-type_rc.c:12:12-12:24)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (16-type_rc.c:12:12-12:24)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:13:3-13:15)
  [Info][Unsound] Write to unknown address: privatization is unsound. (16-type_rc.c:13:3-13:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:28:3-28:9)
  [Info][Unsound] Write to unknown address: privatization is unsound. (16-type_rc.c:28:3-28:9)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Success][Race] Memory location (struct __anonstruct___cancel_jmp_buf_572769531).__mask_was_saved (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __anonunion_pthread_mutexattr_t_488594144).__align (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct _pthread_cleanup_buffer).__canceltype (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (int ) (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Warning][Race] Memory location (struct s).datum (race with conf. 100):
    write with [mhp:{tid=[main, t_fun@16-type_rc.c:27:3-27:37#top]}, thread:[main, t_fun@16-type_rc.c:27:3-27:37#top]] (conf. 100)  (exp: & s->datum) (16-type_rc.c:13:3-13:15)
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __anonunion_pthread_condattr_t_488594145).__align (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_year (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_isdst (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location __daylight@/usr/include/time.h:160:12-160:22 (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_mutex_s).__lock (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_sec (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_min (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_cleanup_frame).__do_it (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_mday (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_mutex_s).__owner (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_wday (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_yday (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_mutex_s).__kind (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_mon (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_rwlock_arch_t).__cur_writer (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_cleanup_frame).__cancel_type (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct sched_param).sched_priority (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct tm).tm_hour (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __anonunion_pthread_barrierattr_t_951761806).__align (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location daylight@/usr/include/time.h:174:12-174:20 (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Success][Race] Memory location (struct __pthread_rwlock_arch_t).__shared (safe):
    write with [mhp:{tid=[main]; created={[main, t_fun@16-type_rc.c:27:3-27:37#top]}}, thread:[main]] (conf. 100)  (exp: & *d) (16-type_rc.c:28:3-28:9)
  [Info][Race] Memory locations race summary:
    safe: 25
    vulnerable: 0
    unsafe: 1
    total memory locations: 26

  $ goblint --disable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --enable allglobs 16-type_rc.c
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:23:3-23:14)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (16-type_rc.c:23:3-23:14)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(s, NoOffset)) (16-type_rc.c:23:3-23:14)
  [Info][Unsound] Unknown address in {&s} has escaped. (16-type_rc.c:23:3-23:14)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (16-type_rc.c:23:3-23:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:24:3-24:16)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:25:3-25:16)
  [Error][Imprecise][Unsound] Function definition missing for get_s (16-type_rc.c:12:12-12:24)
  [Info][Imprecise] INVALIDATING ALL GLOBALS! (16-type_rc.c:12:12-12:24)
  [Info][Imprecise] Invalidating expressions: AddrOf(Var(tmp, NoOffset)) (16-type_rc.c:12:12-12:24)
  [Info][Unsound] Unknown address in {&tmp} has escaped. (16-type_rc.c:12:12-12:24)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (16-type_rc.c:12:12-12:24)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:13:3-13:15)
  [Info][Unsound] Write to unknown address: privatization is unsound. (16-type_rc.c:13:3-13:15)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (16-type_rc.c:28:3-28:9)
  [Info][Unsound] Write to unknown address: privatization is unsound. (16-type_rc.c:28:3-28:9)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Success][Race] Memory location (struct s).datum (safe):
    write with [mhp:{tid=[main, t_fun@16-type_rc.c:27:3-27:37#top]}, thread:[main, t_fun@16-type_rc.c:27:3-27:37#top]] (conf. 100)  (exp: & s->datum) (16-type_rc.c:13:3-13:15)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
