There should be overflow on ILP32:

  $ goblint --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )" --set exp.architecture 32bit 36-svcomp-arch.c
  [Info] Setting "ana.int.interval" to true
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! overflow) )
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in * (36-svcomp-arch.c:6:8-6:17)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  SV-COMP result: unknown

There shouldn't be an overflow on LP64:

  $ goblint --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )" --set exp.architecture 64bit 36-svcomp-arch.c
  [Info] Setting "ana.int.interval" to true
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! overflow) )
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  SV-COMP result: true
