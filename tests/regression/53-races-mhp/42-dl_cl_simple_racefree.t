absence of data-race property disables locked creation analyses:
  $ goblint --enable ana.sv-comp.enabled --enable ana.autotune.enabled --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )" --set ana.autotune.activated[*] reduceAnalyses --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --set ana.activated[+] creationLockset 42-dl_cl_simple_racefree.c
  [Info] Setting "ana.int.interval" to true
  [Info] no specification NoDataRace -> disabling analyses: "race, symb_locks, region, threadDescendants, mustlockHistory, descendantLockset, creationLockset"
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! overflow) )
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (42-dl_cl_simple_racefree.c:12:3-12:11)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (42-dl_cl_simple_racefree.c:17:3-17:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  SV-COMP result: unknown

data-race property keeps locked creation analyses:
  $ goblint --enable ana.sv-comp.enabled --enable ana.autotune.enabled --set ana.specification "CHECK( init(main()), LTL(G ! data-race) )" --set ana.autotune.activated[*] reduceAnalyses --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --set ana.activated[+] creationLockset 42-dl_cl_simple_racefree.c
  [Info] Specification: CHECK( init(main()), LTL(G ! data-race) ) -> enabling soundness analyses: "access, race"
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! data-race) )
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
