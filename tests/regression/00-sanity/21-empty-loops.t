  $ goblint --enable exp.cfgdot 21-empty-loops.c
  [Warning][Deadcode] Function 'suffix' is uncalled: 1 LLoC (21-empty-loops.c:66:1-69:1)
  [Warning][Deadcode] Function 'f_empty_goto_loop_suffix' has dead code:
    on line 76 (21-empty-loops.c:76-76)
  [Warning][Deadcode] Function 'f_empty_while_loop' has dead code:
    on line 64 (21-empty-loops.c:64-64)
  [Warning][Deadcode] Function 'f_empty_while_loop_prefix' has dead code:
    on line 124 (21-empty-loops.c:124-124)
  [Warning][Deadcode] Function 'f_empty_while_loop_semicolon' has dead code:
    on line 139 (21-empty-loops.c:139-139)
  [Warning][Deadcode] Function 'f_empty_while_loop_suffix' has dead code:
    on lines 83..84 (21-empty-loops.c:83-84)
  [Warning][Deadcode] Function 'f_nonempty_while_loop' has dead code:
    on line 104 (21-empty-loops.c:104-104)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 65
    dead: 8 (1 in uncalled functions)
    total: 73
  [Warning][Deadcode][CWE-571] condition '1' is always true (21-empty-loops.c:63:10-63:11)
  [Warning][Deadcode][CWE-571] condition '1' is always true (21-empty-loops.c:81:10-81:11)
  [Warning][Deadcode][CWE-571] condition '1' is always true (21-empty-loops.c:100:10-100:11)
  [Warning][Deadcode][CWE-571] condition '1' is always true (21-empty-loops.c:123:10-123:11)
  [Warning][Deadcode][CWE-571] condition '1' is always true (21-empty-loops.c:136:10-136:11)

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10966185866 -> ret1667 [label = "return"] ;
  	151 -> 10966185866 [label = "Neg(1)"] ;
  	151 -> 151 [label = "skip"] ;
  	fun1667 -> 151 [label = "(body)"] ;
  	10966185866 [fillcolor=orange];
  	151 [];
  	fun1667 [label="f_empty_goto_loop()",shape=box];
  	ret1667 [label="return of f_empty_goto_loop()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_while_loop.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	158 -> ret1668 [label = "return"] ;
  	155 -> 158 [label = "Neg(1)"] ;
  	155 -> 155 [label = "Pos(1)"] ;
  	fun1668 -> 155 [label = "(body)"] ;
  	155 [shape=diamond];
  	158 [fillcolor=orange];
  	fun1668 [label="f_empty_while_loop()",shape=box];
  	ret1668 [label="return of f_empty_while_loop()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_suffix.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10494758249 -> ret1669 [label = "return"] ;
  	160 -> 10494758249 [label = "Neg(1)"] ;
  	162 -> 10494758249 [label = "suffix()"] ;
  	160 -> 160 [label = "skip"] ;
  	fun1669 -> 160 [label = "(body)"] ;
  	160 [];
  	162 [fillcolor=orange];
  	fun1669 [label="f_empty_goto_loop_suffix()",shape=box];
  	ret1669 [label="return of f_empty_goto_loop_suffix()",shape=box,fillcolor=orange];
  	10494758249 [fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_while_loop_suffix.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	171 -> ret1670 [label = "return"] ;
  	170 -> 171 [label = "suffix()"] ;
  	166 -> 170 [label = "Neg(1)"] ;
  	166 -> 166 [label = "Pos(1)"] ;
  	fun1670 -> 166 [label = "(body)"] ;
  	fun1670 [label="f_empty_while_loop_suffix()",shape=box];
  	166 [shape=diamond];
  	170 [fillcolor=orange];
  	171 [fillcolor=orange];
  	ret1670 [label="return of f_empty_while_loop_suffix()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_nonempty_goto_loop.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10142820772 -> ret1671 [label = "return"] ;
  	174 -> 10142820772 [label = "Neg(1)"] ;
  	174 -> 174 [label = "body()"] ;
  	fun1671 -> 174 [label = "(body)"] ;
  	10142820772 [fillcolor=orange];
  	fun1671 [label="f_nonempty_goto_loop()",shape=box];
  	174 [];
  	ret1671 [label="return of f_nonempty_goto_loop()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_nonempty_while_loop.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	184 -> ret1672 [label = "return"] ;
  	179 -> 184 [label = "Neg(1)"] ;
  	182 -> 179 [label = "body()"] ;
  	fun1672 -> 179 [label = "(body)"] ;
  	179 -> 182 [label = "Pos(1)"] ;
  	fun1672 [label="f_nonempty_while_loop()",shape=box];
  	179 [shape=diamond];
  	182 [];
  	184 [fillcolor=orange];
  	ret1672 [label="return of f_nonempty_while_loop()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_prefix.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10451917286 -> ret1673 [label = "return"] ;
  	188 -> 10451917286 [label = "Neg(1)"] ;
  	188 -> 188 [label = "skip"] ;
  	187 -> 188 [label = "prefix()"] ;
  	fun1673 -> 187 [label = "(body)"] ;
  	fun1673 [label="f_empty_goto_loop_prefix()",shape=box];
  	187 [];
  	188 [];
  	ret1673 [label="return of f_empty_goto_loop_prefix()",shape=box,fillcolor=orange];
  	10451917286 [fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_while_loop_prefix.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	197 -> ret1674 [label = "return"] ;
  	194 -> 197 [label = "Neg(1)"] ;
  	194 -> 194 [label = "Pos(1)"] ;
  	190 -> 194 [label = "prefix()"] ;
  	fun1674 -> 190 [label = "(body)"] ;
  	fun1674 [label="f_empty_while_loop_prefix()",shape=box];
  	190 [];
  	194 [shape=diamond];
  	197 [fillcolor=orange];
  	ret1674 [label="return of f_empty_while_loop_prefix()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_semicolon.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10404769065 -> ret1675 [label = "return"] ;
  	198 -> 10404769065 [label = "Neg(1)"] ;
  	198 -> 198 [label = "skip"] ;
  	fun1675 -> 198 [label = "(body)"] ;
  	10404769065 [fillcolor=orange];
  	fun1675 [label="f_empty_goto_loop_semicolon()",shape=box];
  	198 [];
  	ret1675 [label="return of f_empty_goto_loop_semicolon()",shape=box,fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_while_loop_semicolon.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	206 -> ret1676 [label = "return"] ;
  	203 -> 206 [label = "Neg(1)"] ;
  	203 -> 203 [label = "Pos(1)"] ;
  	fun1676 -> 203 [label = "(body)"] ;
  	fun1676 [label="f_empty_while_loop_semicolon()",shape=box];
  	ret1676 [label="return of f_empty_while_loop_semicolon()",shape=box,fillcolor=orange];
  	203 [shape=diamond];
  	206 [fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_multiple.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10218149751 -> ret1677 [label = "return"] ;
  	207 -> 10218149751 [label = "Neg(1)"] ;
  	207 -> 207 [label = "skip"] ;
  	fun1677 -> 207 [label = "(body)"] ;
  	fun1677 [label="f_empty_goto_loop_multiple()",shape=box];
  	ret1677 [label="return of f_empty_goto_loop_multiple()",shape=box,fillcolor=orange];
  	207 [];
  	10218149751 [fillcolor=orange];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_multiple_semicolon_first.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10606643600 -> ret1678 [label = "return"] ;
  	209 -> 10606643600 [label = "Neg(1)"] ;
  	209 -> 209 [label = "skip"] ;
  	fun1678 -> 209 [label = "(body)"] ;
  	10606643600 [fillcolor=orange];
  	fun1678 [label="f_empty_goto_loop_multiple_semicolon_first()",shape=box];
  	ret1678 [label="return of f_empty_goto_loop_multiple_semicolon_first()",shape=box,fillcolor=orange];
  	209 [];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_multiple_semicolon_second.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10984714377 -> ret1679 [label = "return"] ;
  	212 -> 10984714377 [label = "Neg(1)"] ;
  	212 -> 212 [label = "skip"] ;
  	fun1679 -> 212 [label = "(body)"] ;
  	10984714377 [fillcolor=orange];
  	fun1679 [label="f_empty_goto_loop_multiple_semicolon_second()",shape=box];
  	ret1679 [label="return of f_empty_goto_loop_multiple_semicolon_second()",shape=box,fillcolor=orange];
  	212 [];
  }

  $ cat cfgs/21-empty-loops.c/f_empty_goto_loop_multiple_semicolon_both.dot
  digraph cfg {
  	node [id="\N",URL="javascript:show_info('\N');",style=filled,fillcolor=white];
  	10253493141 -> ret1680 [label = "return"] ;
  	215 -> 10253493141 [label = "Neg(1)"] ;
  	215 -> 215 [label = "skip"] ;
  	fun1680 -> 215 [label = "(body)"] ;
  	10253493141 [fillcolor=orange];
  	fun1680 [label="f_empty_goto_loop_multiple_semicolon_both()",shape=box];
  	ret1680 [label="return of f_empty_goto_loop_multiple_semicolon_both()",shape=box,fillcolor=orange];
  	215 [];
  }
