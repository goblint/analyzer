  $ goblint --set ana.activated[+] apron --set ana.relation.privatization mutex-meet --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag 87-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (87-unlock-idx-ambiguous.c:15:3-15:30)
  [Warning][Assert] Assertion "g == 0" is unknown. (87-unlock-idx-ambiguous.c:25:3-25:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-tid --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag 87-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (87-unlock-idx-ambiguous.c:15:3-15:30)
  [Warning][Assert] Assertion "g == 0" is unknown. (87-unlock-idx-ambiguous.c:25:3-25:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-tid-cluster12 --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag 87-unlock-idx-ambiguous.c
  [Warning][Unknown] unlocking mutex (m[def_exc:Unknown int([0,1])]) which may not be held (87-unlock-idx-ambiguous.c:15:3-15:30)
  [Warning][Assert] Assertion "g == 0" is unknown. (87-unlock-idx-ambiguous.c:25:3-25:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

