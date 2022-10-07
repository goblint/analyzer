  $ goblint --enable exp.cfgdot 20-if-0-realnode.c
  [Warning][Deadcode] Function 'stuff' is uncalled: 1 LLoC (20-if-0-realnode.c:4:1-6:1)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 13 (20-if-0-realnode.c:13-13)
    on line 18 (20-if-0-realnode.c:18-18)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 3 (1 in uncalled functions)
    total: 5
  [Warning][Deadcode][CWE-570] condition '0' is always false (20-if-0-realnode.c:11:9-11:10)

  $ cat cfgs/20-if-0-realnode.c/main.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	82 -> ret1669 [label = "return 0"] ;
  	80 -> 82 [label = "stuff()"] ;
  	78 -> 80 [label = "Pos(0)"] ;
  	78 -> 78 [label = "Neg(0)"] ;
  	fun1669 -> 78 [label = "(body)"] ;
  	fun1669 [label="main()",shape=box];
  	ret1669 [label="return of main()",shape=box,fillcolor=orange];
  	78 [shape=diamond];
  	80 [fillcolor=orange];
  	82 [fillcolor=orange];
  }
