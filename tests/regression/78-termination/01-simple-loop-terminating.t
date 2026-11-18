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


  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Success][Witness] invariant confirmed: 1U <= i (01-simple-loop-terminating.c:8:3)
  [Success][Witness] invariant confirmed: i <= 11U (01-simple-loop-terminating.c:8:3)
  [Success][Witness] invariant confirmed: (long long )i >= (long long )\at(i, AnyPrev) + 1LL (01-simple-loop-terminating.c:8:3)
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3

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

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c --set exp.unrolling-factor 1
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info] unrolling loop at 01-simple-loop-terminating.c:8:3-12:3 with factor 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Success][Witness] invariant confirmed: (2U <= i && i <= 11U) || i == 1U (01-simple-loop-terminating.c:8:3)
  [Success][Witness] invariant confirmed: (long long )i >= (long long )\at(i, AnyPrev) + 1LL (01-simple-loop-terminating.c:8:3)
  [Info][Witness] witness validation summary:
    confirmed: 2
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2


Same with octagon:

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable ana.apron.invariant.diff-box --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
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


  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 01-simple-loop-terminating.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Success][Witness] invariant confirmed: 1U <= i (01-simple-loop-terminating.c:8:3)
  [Success][Witness] invariant confirmed: i <= 11U (01-simple-loop-terminating.c:8:3)
  [Success][Witness] invariant confirmed: (long long )i >= (long long )\at(i, AnyPrev) + 1LL (01-simple-loop-terminating.c:8:3)
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
