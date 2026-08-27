  $ goblint --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --disable ana.thread.include-node 56-dl_multiple_creates_sequential_racing.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
  [Warning][Race] Memory location global (race with conf. 110): (56-dl_multiple_creates_sequential_racing.c:4:5-4:15)
    write with Thread id * map * map * map:(map:{}, map:{
                                                        [main] -> {
                                                                    [main, t1] -> {}
                                                                    [main], {t1} -> {}
                                                                  }
                                                      }, map:{
                                                                 mutex -> {[main], [main], {t1}}
                                                               }) (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    write with Thread id * map * map * map:(map:{}, map:{
                                                        [main] -> {
                                                                    [main, t1] -> {}
                                                                    [main], {t1} -> {}
                                                                  }
                                                      }, map:{
                                                                 mutex -> {[main], {t1}}
                                                               }) (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    write with [Thread id * map * map * map:(map:{}, map:{
                                                         [main] -> {
                                                                     [main, t1] -> {}
                                                                     [main], {t1} -> {}
                                                                   }
                                                       }, map:{
                                                                  mutex -> {[main, t1]}
                                                                }), thread:[main, t1]] (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    write with [Thread id * map * map * map:(map:{}, map:{
                                                         [main] -> {
                                                                     [main, t1] -> {}
                                                                     [main], {t1} -> {}
                                                                   }
                                                       }, map:{
                                                                  mutex -> {[main, t1], [main]}
                                                                }), thread:[main, t1]] (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    write with [Thread id * map * map * map:(map:{
                                                 [main, t1] -> {}
                                                 [main], {t1} -> {}
                                               }, map:{}, map:{
                                                                  mutex -> {[main]}
                                                                }), mhp:{created={[main, t1], [main], {t1}}}, lock:{mutex}, thread:[main]] (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:20:3-20:11)
    read with Thread id * map * map * map:(map:{}, map:{
                                                       [main] -> {
                                                                   [main, t1] -> {}
                                                                   [main], {t1} -> {}
                                                                 }
                                                     }, map:{
                                                                mutex -> {[main], [main], {t1}}
                                                              }) (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    read with Thread id * map * map * map:(map:{}, map:{
                                                       [main] -> {
                                                                   [main, t1] -> {}
                                                                   [main], {t1} -> {}
                                                                 }
                                                     }, map:{
                                                                mutex -> {[main], {t1}}
                                                              }) (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    read with [Thread id * map * map * map:(map:{}, map:{
                                                        [main] -> {
                                                                    [main, t1] -> {}
                                                                    [main], {t1} -> {}
                                                                  }
                                                      }, map:{
                                                                 mutex -> {[main, t1]}
                                                               }), thread:[main, t1]] (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    read with [Thread id * map * map * map:(map:{}, map:{
                                                        [main] -> {
                                                                    [main, t1] -> {}
                                                                    [main], {t1} -> {}
                                                                  }
                                                      }, map:{
                                                                 mutex -> {[main, t1], [main]}
                                                               }), thread:[main, t1]] (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:11:3-11:11)
    read with [Thread id * map * map * map:(map:{
                                                [main, t1] -> {}
                                                [main], {t1} -> {}
                                              }, map:{}, map:{
                                                                 mutex -> {[main]}
                                                               }), mhp:{created={[main, t1], [main], {t1}}}, lock:{mutex}, thread:[main]] (conf. 110)  (exp: & global) (56-dl_multiple_creates_sequential_racing.c:20:3-20:11)
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 1
    total memory locations: 4
