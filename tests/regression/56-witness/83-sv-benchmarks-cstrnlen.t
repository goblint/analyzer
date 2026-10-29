  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 83-sv-benchmarks-cstrnlen.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Warning][Deadcode][CWE-571] condition 'maxlen != 0UL' (possibly inserted by CIL) is always true (83-sv-benchmarks-cstrnlen.c:43:15-43:43)
  [Warning][Deadcode][CWE-570] condition '(int const   )*cp != 0' (possibly inserted by CIL) is always false (83-sv-benchmarks-cstrnlen.c:43:15-43:43)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 2
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "1 <= maxlen" in cstrnlen.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-sv-benchmarks-cstrnlen.c
          line: 43
          column: 2
          function: cstrnlen
        value: 1UL <= maxlen
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-sv-benchmarks-cstrnlen.c
          line: 43
          column: 2
          function: cstrnlen
        value: maxlen <= 2147483647UL
        format: c_expression
