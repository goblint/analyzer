TODO: Should not outout Apron overflow warning about term variable? ID is probably not stable.

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: term8_3_id0#1129
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 3
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-simple-loop-terminating.c
          line: 8
          column: 3
          function: main
        value: 1U <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-simple-loop-terminating.c
          line: 8
          column: 3
          function: main
        value: i <= 11U
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 01-simple-loop-terminating.c
          line: 8
          column: 3
          function: main
        value: (long long )\at(i, AnyPrev) >= 1LL && (long long )i >= (long long )\at(i,
          AnyPrev) + 1LL
        format: ext_c_expression
