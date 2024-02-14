  $ goblint --set ana.activated "['base', 'mallocWrapper']" --set ana.ctx_insens[+] 'base' --set ana.ctx_insens[+] 'mallocWrapper' --set ana.base.privatization none --enable warn.debug 87-casts-dep-on-param.c 2>&1 | sed -r 's/sid:[0-9]+/sid:$SID/' | tee default-output.txt
  Option warning: Without thread escape analysis, every local variable whose address is taken is considered escaped, i.e., global!
  [Info][Unsound] Unknown address in {&i} has escaped. (87-casts-dep-on-param.c:11:3-11:11)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (87-casts-dep-on-param.c:11:3-11:11)
  [Debug][Analyzer] Base EvalInt i query answering bot instead of {?, NULL, &(alloc@sid:$SID)} (87-casts-dep-on-param.c:13:7-13:15)
  [Debug][Analyzer] Base EvalInt i query answering bot instead of {?, NULL, &(alloc@sid:$SID)} (87-casts-dep-on-param.c:14:3-14:11)
  [Info][Unsound] Unknown address in {&p} has escaped. (87-casts-dep-on-param.c:17:7-17:19)
  [Info][Unsound] Unknown address in {&i} has escaped. (87-casts-dep-on-param.c:17:7-17:19)
  [Info][Unsound] Unknown value in {?} could be an escaped pointer address! (87-casts-dep-on-param.c:17:7-17:19)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15

  $ goblint --set ana.activated "['base', 'mallocWrapper']" --set ana.ctx_insens[+] 'base' --set ana.ctx_insens[+] 'mallocWrapper' --set ana.base.privatization none --enable warn.debug --enable dbg.full-output 87-casts-dep-on-param.c 2>&1 | sed -r 's/sid:[0-9]+/sid:$SID/' > full-output.txt

  $ diff default-output.txt full-output.txt
  4,5c4,5
  < [Debug][Analyzer] Base EvalInt i query answering bot instead of {?, NULL, &(alloc@sid:$SID)} (87-casts-dep-on-param.c:13:7-13:15)
  < [Debug][Analyzer] Base EvalInt i query answering bot instead of {?, NULL, &(alloc@sid:$SID)} (87-casts-dep-on-param.c:14:3-14:11)
  ---
  > [Debug][Analyzer] Base EvalInt i query answering bot instead of {?, NULL, &(alloc@sid:$SID@tid:Top Threads(#top))} (87-casts-dep-on-param.c:13:7-13:15)
  > [Debug][Analyzer] Base EvalInt i query answering bot instead of {?, NULL, &(alloc@sid:$SID@tid:Top Threads(#top))} (87-casts-dep-on-param.c:14:3-14:11)
  [1]
