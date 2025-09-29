  $ goblint --set ana.activated[*] var_eq --set ana.activated[+] taintPartialContexts --set result pretty-deterministic --set outfile pretty.txt 53-var_eq-enter-reachable.c
  [Warning] Without thread escape analysis, every local variable whose address is taken is considered escaped, i.e., global!
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

TODO: Should not have unreachable equality x == y in foo:

  $ cat pretty.txt
  Mapping {
    53-var_eq-enter-reachable.c:6:1-6:1(foo) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    53-var_eq-enter-reachable.c:11:3-11:8(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    53-var_eq-enter-reachable.c:12:3-12:8(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{{x, y}}], map:{})}
    53-var_eq-enter-reachable.c:13:10-13:11(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    53-var_eq-enter-reachable.c:4:1-6:1(foo) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    53-var_eq-enter-reachable.c:8:1-14:1(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    53-var_eq-enter-reachable.c:4:1-6:1(foo) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    53-var_eq-enter-reachable.c:8:1-14:1(main) ->
      PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[var_eq:{}], map:{})}
    OTHERS -> Not available
  }
