  $ goblint --enable warn.deterministic 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test ancient solvers:

  $ goblint --enable warn.deterministic --set solver WL 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver effectWConEq 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test topdown solvers:

  $ goblint --enable warn.deterministic --set solver topdown_deprecated 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown_term 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown_space_cache_term 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver td3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test SLR solvers:

  $ goblint --enable warn.deterministic --set solver widen1 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver widen2 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver widen3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver two 01-assert.c
  [Error] Fixpoint not reached at L:entry state of main (299) on 01-assert.c:4:1-15:1
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                                                                                   mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                                                                                   base:({
                                                                                                                           }, {}, {}, {}),
                                                                                                                   threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                                                                                   threadflag:Singlethreaded,
                                                                                                                   threadreturn:true,
                                                                                                                   escape:{},
                                                                                                                   mutexEvents:(),
                                                                                                                   access:(),
                                                                                                                   mutex:(lockset:{}, multiplicity:{}),
                                                                                                                   race:(),
                                                                                                                   mhp:(),
                                                                                                                   assert:(),
                                                                                                                   pthreadMutexType:()], map:{})}
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                                                                                               mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                                                                                               base:({
                                                                                                                                       }, {}, {}, {}),
                                                                                                                               threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                                                                                               threadflag:Singlethreaded,
                                                                                                                               threadreturn:true,
                                                                                                                               escape:{},
                                                                                                                               mutexEvents:(),
                                                                                                                               access:(),
                                                                                                                               mutex:(lockset:{}, multiplicity:{}),
                                                                                                                               race:(),
                                                                                                                               mhp:(),
                                                                                                                               assert:(),
                                                                                                                               pthreadMutexType:()], map:{})} instead of bot
  
  [Error] Fixpoint not reached at L:node 1 "success = 1;" on 01-assert.c:5:7-5:18
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 2 "silence = 1;" on 01-assert.c:6:7-6:18
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 3 "fail = 0;" on 01-assert.c:7:7-7:15
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 4 "__goblint_assert(success);" on 01-assert.c:10:3-10:28
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 5 "__goblint_assert(unknown == 4);" on 01-assert.c:11:3-11:33
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 6 "__goblint_assert(fail);" on 01-assert.c:12:3-12:25
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 7 "return (0);" on 01-assert.c:13:10-13:11
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node 9 "__goblint_assert(silence);" on 01-assert.c:14:3-14:28
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:node -299 "return;" on 01-assert.c:15:1-15:1
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Error] Fixpoint not reached at L:call of main (299) on 01-assert.c:4:1-15:1
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):Dead code instead of bot
  
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' is uncalled: 8 LLoC (01-assert.c:4:1-15:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 0
    dead: 8 (8 in uncalled functions)
    total lines: 8
  [Error][Unsound] Fixpoint not reached
  [3]

  $ goblint --enable warn.deterministic --set solver new 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr+ 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr1 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr2 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr4 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr1p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9
  $ goblint --enable warn.deterministic --set solver slr2p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr4p 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3t 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver slr3tp 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9
