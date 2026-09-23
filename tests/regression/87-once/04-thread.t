  $ goblint --set ana.activated[+] pthreadOnce --set ana.race.graph-coloring greedy 04-thread.c
  [Info][Imprecise] Invalidating expressions: & tmp (04-thread.c:31:13-31:59)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 0
    total lines: 29
  [Warning][Race] Memory location g (race with conf. 110): (04-thread.c:5:5-5:6)
    Safe subset 1:
      write with [onces:(active:{}, seen:{i_once}), thread:[main, t_fun@04-thread.c:54:3-54:40, t_other@04-thread.c:31:13-31:59]] (conf. 110)  (exp: & g) (04-thread.c:13:3-13:9)
    Safe subset 2:
      write with [onces:(active:{}, seen:{i_once}), thread:[main, t_other@04-thread.c:31:13-31:59]] (conf. 110)  (exp: & g) (04-thread.c:13:3-13:9)
    Safe subset 3:
      write with [onces:(active:{once}, seen:{i_once}), mhp:{created={[main, t_fun@04-thread.c:54:3-54:40, t_other@04-thread.c:31:13-31:59]}}, thread:[main, t_fun@04-thread.c:54:3-54:40]] (conf. 110)  (exp: & g) (04-thread.c:32:3-32:9)
      write with [onces:(active:{once}, seen:{i_once}), mhp:{created={[main, t_other@04-thread.c:31:13-31:59], [main, t_fun@04-thread.c:54:3-54:40]}}, thread:[main]] (conf. 110)  (exp: & g) (04-thread.c:32:3-32:9)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3
