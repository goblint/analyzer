  $ goblint --enable ana.int.interval --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )"  32-no-ov.c
  SV-COMP specification: CHECK( init(main()), LTL(G ! overflow) )
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow (32-no-ov.c:5:6-5:159)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow (32-no-ov.c:5:6-5:159)
  [Warning][Integer > Overflow][CWE-191] Unsigned integer underflow (32-no-ov.c:5:6-5:159)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow (32-no-ov.c:5:6-5:159)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 0
    total lines: 3
  SV-COMP result: true
