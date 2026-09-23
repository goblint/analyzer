  $ goblint --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --set ana.race.graph-coloring greedy 03-funloop_simple.c
  [Info][Assumption] Mutexes are non-recursive by default (03-funloop_simple.c:25:5-25:50)
  [Warning][Unknown] unlocking mutex (cache[def_exc:Unknown int([-31,31])].refs_mutex) which may not be held (03-funloop_simple.c:13:3-13:43)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Warning][Race] Memory location cache[?].refs (race with conf. 110): (03-funloop_simple.c:8:3-8:12)
    Safe subset 1:
      write with [symblock:{p-lock:*.refs_mutex}, thread:[main, t_fun@03-funloop_simple.c:29:3-29:40]] (conf. 110)  (exp: & entry->refs) (03-funloop_simple.c:12:3-12:16)
      read with [symblock:{p-lock:*.refs_mutex}, thread:[main, t_fun@03-funloop_simple.c:29:3-29:40]] (conf. 110)  (exp: & entry->refs) (03-funloop_simple.c:12:3-12:16)
    Safe subset 2:
      write with [mhp:{created={[main, t_fun@03-funloop_simple.c:29:3-29:40]}}, thread:[main]] (conf. 110)  (exp: & cache[5].refs) (03-funloop_simple.c:32:3-32:18)
      read with [mhp:{created={[main, t_fun@03-funloop_simple.c:29:3-29:40]}}, thread:[main]] (conf. 110)  (exp: & cache[5].refs) (03-funloop_simple.c:32:3-32:18)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1
  [Info][Assumption] Mutexes are non-recursive by default
