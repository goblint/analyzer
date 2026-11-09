  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: $TERM
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 2
    flow-insensitive invariants: 0
    loop transition invariants: 1
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
        value: (long long )i >= (long long )\at(i, AnyPrev) + 1LL
        format: ext_c_expression


Same with loop unrolling:

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c --set exp.unrolling-factor 1 | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info] unrolling loop at 01-simple-loop-terminating.c:8:3-12:3 with factor 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: $TERM
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    loop transition invariants: 1
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
        value: (2U <= i && i <= 11U) || i == 1U
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 01-simple-loop-terminating.c
          line: 8
          column: 3
          function: main
        value: (long long )i >= (long long )\at(i, AnyPrev) + 1LL
        format: ext_c_expression
