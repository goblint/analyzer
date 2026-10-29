  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 80-sv-benchmarks-strlen.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Warning][Deadcode][CWE-570] condition '! s' is always false (80-sv-benchmarks-strlen.c:13:7-13:9)
  [Warning][Deadcode][CWE-570] condition '*s' (possibly inserted by CIL) is always false (80-sv-benchmarks-strlen.c:14:11-14:15)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "i == 0" in strlen.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-sv-benchmarks-strlen.c
          line: 14
          column: 3
          function: strlen
        value: i == 0UL
        format: c_expression
