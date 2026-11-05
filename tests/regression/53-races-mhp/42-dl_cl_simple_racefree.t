absence of data-race property disables locked creation analyses:
  $ goblint --set dbg.level debug --enable ana.autotune.enabled --set ana.autotune.activated[+] reduceAnalyses --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --set ana.activated[+] creationLockset 42-dl_cl_simple_racefree.c 2>&1 | grep -E -c "Activated analyses: [[:print:]]*creationLockset"
  0
  [1]

data-race property enables locked creation analyses:
  $ goblint --enable ana.sv-comp.enabled --enable ana.autotune.enabled --set ana.specification "CHECK( init(main()), LTL(G ! data-race) )" --set ana.autotune.activated[+] concurrencySafetySpecification 42-dl_cl_simple_racefree.c 2>&1 | grep -c "Specification: NoDataRace -> enabling thread analyses: \"[[:print:]]*threadDescendants, mustlockHistory, descendantLockset, creationLockset"
  1
