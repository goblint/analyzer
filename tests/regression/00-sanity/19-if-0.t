  $ goblint --enable exp.cfgdot 19-if-0.c
  [Success][Assert] Assertion "1" will succeed (19-if-0.c:15:9-15:27)
  [Warning][Deadcode] Function 'stuff' is uncalled: 1 LLoC (19-if-0.c:3:1-5:1)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 11 (19-if-0.c:11-11)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 2 (1 in uncalled functions)
    total: 6
  [Warning][Deadcode][CWE-570] condition '0' is always false (19-if-0.c:9:9-9:10)

  $ cat cfgs/19-if-0.c/main.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	83 -> ret1669 [label = "return 0"] ;
  	82 -> 83 [label = "__goblint_check(1)"] ;
  	80 -> 83 [label = "stuff()"] ;
  	78 -> 82 [label = "Neg(0)"] ;
  	fun1669 -> 78 [label = "(body)"] ;
  	78 -> 80 [label = "Pos(0)"] ;
  	fun1669 [label="main()",shape=box];
  	ret1669 [label="return of main()",shape=box];
  	78 [shape=diamond];
  	80 [fillcolor=orange];
  	82 [];
  	83 [];
  }
