  $ goblint --set ana.activated[+] mallocFresh --set ana.activated[-] mhp --set ana.thread.domain plain 51-fresh-global.c | sed -r 's/sid:[0-9]+/sid:$SID/'
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location (alloc@sid:$SID@tid:main) (race with conf. 110): (51-fresh-global.c:25:7-25:31)
    write with lock:{A} (conf. 110)  (exp: & *i) (51-fresh-global.c:10:3-10:10)
    write with thread:main (conf. 110)  (exp: & *i) (51-fresh-global.c:27:3-27:9)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2

  $ goblint --set ana.activated[+] mallocFresh --set ana.activated[-] mhp --set ana.thread.domain plain --enable dbg.full-output 51-fresh-global.c | sed -r 's/sid:[0-9]+/sid:$SID/'
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location (alloc@sid:$SID@tid:main(#top)) (race with conf. 110): (51-fresh-global.c:25:7-25:31)
    write with lock:{A} (conf. 110)  (exp: & *i) (51-fresh-global.c:10:3-10:10)
    write with thread:main (conf. 110)  (exp: & *i) (51-fresh-global.c:27:3-27:9)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 1
    total memory locations: 2
