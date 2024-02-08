  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' issue-1356.c
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow (issue-1356.c:10:3-10:53)
  [Warning][Integer > Overflow][CWE-190][CWE-191] Signed integer overflow and underflow (issue-1356.c:11:3-11:15)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
  [Info][Witness] witness generation summary:
    total generation entries: 0

  $ yamlWitnessStrip < witness.yml
  []
