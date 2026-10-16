  $ goblint --set ana.activated[+] 'pthreadBarriers' 09-race.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
  [Warning][Race] Memory location h (race with conf. 110): (09-race.c:8:5-8:6)
    write with thread:[main, f1@09-race.c:29:5-29:36], barriers:(mayBarriers:{}, must observed:{}) (conf. 110)  (exp: & h) (09-race.c:15:5-15:10)
    write with thread:[main], mhp:{created={[main, f1@09-race.c:29:5-29:36]}}, barriers:(mayBarriers:{}, must observed:{}) (conf. 110)  (exp: & h) (09-race.c:31:5-31:10)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
