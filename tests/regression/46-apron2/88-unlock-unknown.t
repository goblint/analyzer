  $ goblint --set ana.activated[+] apron --set ana.relation.privatization mutex-meet --enable ana.sv-comp.functions 88-unlock-unknown.c
  [Warning][Unknown] unlocking unknown mutex which may not be held (88-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (88-unlock-unknown.c:12:3-12:26)
  [Warning][Assert] Assertion "g == 0" is unknown. (88-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-tid --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag 88-unlock-unknown.c
  [Warning][Unknown] unlocking unknown mutex which may not be held (88-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (88-unlock-unknown.c:12:3-12:26)
  [Warning][Assert] Assertion "g == 0" is unknown. (88-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-tid-cluster12 --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag 88-unlock-unknown.c
  [Warning][Unknown] unlocking unknown mutex which may not be held (88-unlock-unknown.c:12:3-12:26)
  [Warning][Unknown] unlocking NULL mutex (88-unlock-unknown.c:12:3-12:26)
  [Warning][Assert] Assertion "g == 0" is unknown. (88-unlock-unknown.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

