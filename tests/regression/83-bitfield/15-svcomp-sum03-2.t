  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --enable ana.int.bitfield 15-svcomp-sum03-2.c
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (15-svcomp-sum03-2.c:11:5-11:18)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (15-svcomp-sum03-2.c:12:5-12:8)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on line 14 (15-svcomp-sum03-2.c:14-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 1
    total lines: 9
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (15-svcomp-sum03-2.c:10:9-10:10)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content: []
