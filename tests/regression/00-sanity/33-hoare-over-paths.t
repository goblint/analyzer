  $ goblint --set ana.path_sens[+] mutex --set result pretty --set outfile pretty.txt 33-hoare-over-paths.c
  [Success][Assert] Assertion "1" will succeed (33-hoare-over-paths.c:11:5-11:24)
  [Success][Assert] Assertion "1" will succeed (33-hoare-over-paths.c:16:5-16:24)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7

  $ cat pretty.txt
  Mapping {
    33-hoare-over-paths.c:9:7-9:8(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->   ⊤
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:10:5-10:10(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->
                                                                       (Not {0}([-31,31]))
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:11:5-11:24(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:15:5-15:27(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:16:5-16:24(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{m}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:33:10-33:11(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{m}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{}),
                                                   (MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Local {
                                                                     r ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:7:1-34:1(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Temp {
                                                                     RETURN ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{m}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{}),
                                                   (MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                   Temp {
                                                                     RETURN ->   0
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    33-hoare-over-paths.c:7:1-34:1(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                           mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                           base:({
                                                                   Global {
                                                                     m ->   mutex
                                                                   }
                                                                 }, {}, {}, {}),
                                                           threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                           threadflag:Singlethreaded,
                                                           threadreturn:True,
                                                           escape:{},
                                                           mutexEvents:(),
                                                           access:(),
                                                           mutex:(lockset:{}, multiplicity:{}),
                                                           race:(),
                                                           mhp:(),
                                                           assert:(),
                                                           pthreadMutexType:()], map:{})}
    OTHERS -> Not available
  }
