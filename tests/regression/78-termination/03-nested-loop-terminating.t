  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 03-nested-loop-terminating.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/' | sed -r 's/\\at\(term[0-9]+_[0-9]+_id[0-9]+, AnyPrev\)#-?[0-9]+/$TERM_ANYPREV/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (03-nested-loop-terminating.c:11:10-11:19)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (03-nested-loop-terminating.c:11:10-11:19)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: $TERM
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: $TERM_ANYPREV
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 10
    flow-insensitive invariants: 0
    loop transition invariants: 0
    total generation entries: 1

No wrong transition invariant `j >= \at(j, AnyPrev) + 1` for inner loop.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 11
          column: 3
          function: main
        value: 1U <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 11
          column: 3
          function: main
        value: columns == 4U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 11
          column: 3
          function: main
        value: i <= 4U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 11
          column: 3
          function: main
        value: rows == 3U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 16
          column: 5
          function: main
        value: 1 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 16
          column: 5
          function: main
        value: 1U <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 16
          column: 5
          function: main
        value: columns == 4U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 16
          column: 5
          function: main
        value: i <= 3U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 16
          column: 5
          function: main
        value: j <= 5
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-nested-loop-terminating.c
          line: 16
          column: 5
          function: main
        value: rows == 3U
        format: c_expression
