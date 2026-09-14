With ana.int.interval_threshold_widening enabled, the analysis is precise enough to verify that reach_error is unreachable.

  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval --enable ana.int.interval_threshold_widening 02-mine2017-ex4.6.c
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (02-mine2017-ex4.6.c:9:1-9:43)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 1 (1 in uncalled functions)
    total lines: 10
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (02-mine2017-ex4.6.c:10:39-10:46)

Similar to svcomp26/level03, with some unrolling and branchSet.

  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval --enable ana.int.interval_threshold_widening --set ana.activated[+] branchSet --set exp.unrolling-factor 10 --set dbg.timeout 5 02-mine2017-ex4.6.c
  [Info] unrolling loop at 02-mine2017-ex4.6.c:14:3-18:3 with factor 10
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (02-mine2017-ex4.6.c:9:1-9:43)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 1 (1 in uncalled functions)
    total lines: 10
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (02-mine2017-ex4.6.c:10:39-10:46)

With branchSet context-insensitive, the analysis shouldn't suddenly be extremely slow.

  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval --enable ana.int.interval_threshold_widening --set ana.activated[+] branchSet --set ana.ctx_insens[+] branchSet --set exp.unrolling-factor 10 --set dbg.timeout 5 02-mine2017-ex4.6.c
  [Info] unrolling loop at 02-mine2017-ex4.6.c:14:3-18:3 with factor 10
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (02-mine2017-ex4.6.c:9:1-9:43)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 1 (1 in uncalled functions)
    total lines: 10
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (02-mine2017-ex4.6.c:10:39-10:46)
