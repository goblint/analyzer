  $ goblint --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --set ana.race.graph-coloring greedy --enable warn.deterministic 37-funloop_index.c
  [Warning][Race] Memory location cache[?].refs (race with conf. 110): (37-funloop_index.c:9:3-9:12)
    Safe subset 1:
      write with [thread:[main, t_fun@37-funloop_index.c:32:3-32:40], symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:13:3-13:16)
      write with [thread:[main, t_fun@37-funloop_index.c:32:3-32:40], symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:14:4-14:18)
      write with [thread:[main, t_fun@37-funloop_index.c:32:3-32:40]] (conf. 110)  (exp: & (entry + 0)->refs) (37-funloop_index.c:15:3-15:18)
      read with [thread:[main, t_fun@37-funloop_index.c:32:3-32:40], symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:13:3-13:16)
      read with [thread:[main, t_fun@37-funloop_index.c:32:3-32:40], symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:14:4-14:18)
      read with [thread:[main, t_fun@37-funloop_index.c:32:3-32:40]] (conf. 110)  (exp: & (entry + 0)->refs) (37-funloop_index.c:15:3-15:18)
    Safe subset 2:
      write with [thread:[main], mhp:{created={[main, t_fun@37-funloop_index.c:32:3-32:40]}}, symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:13:3-13:16)
      write with [thread:[main], mhp:{created={[main, t_fun@37-funloop_index.c:32:3-32:40]}}, symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:14:4-14:18)
      write with [thread:[main], mhp:{created={[main, t_fun@37-funloop_index.c:32:3-32:40]}}] (conf. 110)  (exp: & (entry + 0)->refs) (37-funloop_index.c:15:3-15:18)
      read with [thread:[main], mhp:{created={[main, t_fun@37-funloop_index.c:32:3-32:40]}}, symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:13:3-13:16)
      read with [thread:[main], mhp:{created={[main, t_fun@37-funloop_index.c:32:3-32:40]}}, symblock:{p-lock:*.refs_mutex}] (conf. 110)  (exp: & entry->refs) (37-funloop_index.c:14:4-14:18)
      read with [thread:[main], mhp:{created={[main, t_fun@37-funloop_index.c:32:3-32:40]}}] (conf. 110)  (exp: & (entry + 0)->refs) (37-funloop_index.c:15:3-15:18)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total lines: 18
  [Warning][Unknown] unlocking mutex (cache[def_exc:Unknown int([-31,31])].refs_mutex) which may not be held (37-funloop_index.c:16:3-16:43)
  [Info][Assumption] Mutexes are non-recursive by default (37-funloop_index.c:28:5-28:50)
  [Info][Assumption] Mutexes are non-recursive by default
