  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 81-sv-benchmarks-strspn.c
  [Warning][Deadcode] Function 'strspn' has dead code:
    on line 17 (81-sv-benchmarks-strspn.c:17-17)
    on line 19 (81-sv-benchmarks-strspn.c:19-19)
    on line 22 (81-sv-benchmarks-strspn.c:22-22)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 3
    total lines: 18
  [Warning][Deadcode][CWE-570] condition '*s' (possibly inserted by CIL) is always false (81-sv-benchmarks-strspn.c:16:8-16:12)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "l == 0" in strspn.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 81-sv-benchmarks-strspn.c
          line: 16
          column: 3
          function: strspn
        value: l == 0UL
        format: c_expression
