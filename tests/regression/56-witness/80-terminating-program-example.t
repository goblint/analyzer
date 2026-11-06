TODO: Should not outout Apron overflow warning about term variable? ID is probably not stable.

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 80-terminating-program-example.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: term15_5_id0#658
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 5
    flow-insensitive invariants: 0
    loop transition invariants: 1
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-terminating-program-example.c
          line: 15
          column: 5
          function: main
        value: (long long )j + (long long )i == 6LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-terminating-program-example.c
          line: 15
          column: 5
          function: main
        value: 1 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-terminating-program-example.c
          line: 15
          column: 5
          function: main
        value: 3 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-terminating-program-example.c
          line: 15
          column: 5
          function: main
        value: i <= 5
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-terminating-program-example.c
          line: 15
          column: 5
          function: main
        value: j <= 3
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 80-terminating-program-example.c
          line: 15
          column: 5
          function: main
        value: (long long )\at(i, AnyPrev) >= (long long )i + 1LL
        format: ext_c_expression

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 80-terminating-program-example.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: 1 <= j (80-terminating-program-example.c:15:5)
  [Success][Witness] invariant confirmed: 3 <= i (80-terminating-program-example.c:15:5)
  [Success][Witness] invariant confirmed: i <= 5 (80-terminating-program-example.c:15:5)
  [Success][Witness] invariant confirmed: j <= 3 (80-terminating-program-example.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j + (long long )i == 6LL (80-terminating-program-example.c:15:5)
  [Warning][Witness] cannot validate invariant of type loop_transition_invariant
  [Info][Witness] witness validation summary:
    confirmed: 5
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 1
    disabled: 0
    total validation entries: 6

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate 80-terminating-program-example.witness-2.1.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 80-terminating-program-example.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Witness] cannot validate invariant of type loop_transition_invariant
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 1
    disabled: 0
    total validation entries: 1
