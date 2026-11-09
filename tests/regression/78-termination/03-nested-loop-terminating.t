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

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 03-nested-loop-terminating.c --enable warn.deterministic
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (03-nested-loop-terminating.c:11:10-11:19)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
  [Info][Witness] witness validation summary:
    confirmed: 10
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 10
  [Success][Witness] invariant confirmed: 1U <= i (03-nested-loop-terminating.c:11:3)
  [Success][Witness] invariant confirmed: columns == 4U (03-nested-loop-terminating.c:11:3)
  [Success][Witness] invariant confirmed: i <= 4U (03-nested-loop-terminating.c:11:3)
  [Success][Witness] invariant confirmed: rows == 3U (03-nested-loop-terminating.c:11:3)
  [Success][Witness] invariant confirmed: 1 <= j (03-nested-loop-terminating.c:16:5)
  [Success][Witness] invariant confirmed: 1U <= i (03-nested-loop-terminating.c:16:5)
  [Success][Witness] invariant confirmed: columns == 4U (03-nested-loop-terminating.c:16:5)
  [Success][Witness] invariant confirmed: i <= 3U (03-nested-loop-terminating.c:16:5)
  [Success][Witness] invariant confirmed: j <= 5 (03-nested-loop-terminating.c:16:5)
  [Success][Witness] invariant confirmed: rows == 3U (03-nested-loop-terminating.c:16:5)
  [Warning][Termination] The program might not terminate! (Loop analysis) (03-nested-loop-terminating.c:11:10-11:19)

Wrong invariant from handwritten witness should not be confirmed:

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate 03-nested-loop-terminating.bad.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 03-nested-loop-terminating.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (03-nested-loop-terminating.c:11:10-11:19)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (03-nested-loop-terminating.c:11:10-11:19)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
  [Warning][Witness] invariant unconfirmed: j >= \at(j, AnyPrev) + 1 (03-nested-loop-terminating.c:16:5)
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1

