  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable dbg.termination-bounds --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 83-terminating-program-example-10000.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Success][Termination] Loop terminates: bounded by IntDomLifter(intdomtuple):(Unknown int([0,15]),[0,5000]) iteration(s) (83-terminating-program-example-10000.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: $TERM
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 5
    flow-insensitive invariants: 0
    loop transition invariants: 1
    total generation entries: 1

TODO: Why no transition invariant on j, like with octagons (below)?

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: (long long )j + (long long )i == 10001LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: 1 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: 5000 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: i <= 10000
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: j <= 5001
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: (long long )\at(i, AnyPrev) >= (long long )i + 1LL
        format: ext_c_expression

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable dbg.termination-bounds --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 83-terminating-program-example-10000.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Success][Termination] Loop terminates: bounded by IntDomLifter(intdomtuple):(Unknown int([0,15]),[0,5000]) iteration(s) (83-terminating-program-example-10000.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: 1 <= j (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: 5000 <= i (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: i <= 10000 (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: j <= 5001 (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j + (long long )i == 10001LL (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: (long long )\at(i, AnyPrev) >= (long long )i + 1LL (83-terminating-program-example-10000.c:15:5)
  [Info][Witness] witness validation summary:
    confirmed: 6
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 6

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable dbg.termination-bounds --set witness.yaml.validate 83-terminating-program-example-10000.witness-2.1.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 83-terminating-program-example-10000.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Success][Termination] Loop terminates: bounded by IntDomLifter(intdomtuple):(Unknown int([0,15]),[0,5000]) iteration(s) (83-terminating-program-example-10000.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: \at(i, AnyPrev) > i (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: i - j < \at(i, AnyPrev) - \at(j, AnyPrev) (83-terminating-program-example-10000.c:15:1)
  [Success][Witness] invariant confirmed: i - j <= \at(i, AnyPrev) - \at(j, AnyPrev) (83-terminating-program-example-10000.c:15:1)
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3


Same with octagon:

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable dbg.termination-bounds --enable ana.apron.invariant.diff-box --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 83-terminating-program-example-10000.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Success][Termination] Loop terminates: bounded by IntDomLifter(intdomtuple):(Unknown int([0,15]),[0,5000]) iteration(s) (83-terminating-program-example-10000.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Analyzer] Invariant Apron: cannot convert to cil var in overflow preserving manner: $TERM
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 5
    flow-insensitive invariants: 0
    loop transition invariants: 4
    total generation entries: 1

TODO: Are transition invariants between i and j useful/avoidable?

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: (long long )j + (long long )i == 10001LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: 1 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: 5000 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: i <= 10000
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: j <= 5001
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: (long long )\at(i, AnyPrev) >= (long long )i + 1LL
        format: ext_c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: (long long )j + (long long )\at(i, AnyPrev) >= 10002LL
        format: ext_c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: (long long )j >= (long long )\at(j, AnyPrev) + 1LL
        format: ext_c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 83-terminating-program-example-10000.c
          line: 15
          column: 5
          function: main
        value: 10000LL >= (long long )i + (long long )\at(j, AnyPrev)
        format: ext_c_expression

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable dbg.termination-bounds --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 83-terminating-program-example-10000.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Success][Termination] Loop terminates: bounded by IntDomLifter(intdomtuple):(Unknown int([0,15]),[0,5000]) iteration(s) (83-terminating-program-example-10000.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: 1 <= j (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: 5000 <= i (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: i <= 10000 (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: j <= 5001 (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j + (long long )i == 10001LL (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: 10000LL >= (long long )i + (long long )\at(j, AnyPrev) (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j + (long long )\at(i, AnyPrev) >= 10002LL (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: (long long )\at(i, AnyPrev) >= (long long )i + 1LL (83-terminating-program-example-10000.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j >= (long long )\at(j, AnyPrev) + 1LL (83-terminating-program-example-10000.c:15:5)
  [Info][Witness] witness validation summary:
    confirmed: 9
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 9

TODO: Why not invariants confirmed?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable dbg.termination-bounds --set witness.yaml.validate 83-terminating-program-example-10000.witness-2.1.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 83-terminating-program-example-10000.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Success][Termination] Loop terminates: bounded by IntDomLifter(intdomtuple):(Unknown int([0,15]),[0,5000]) iteration(s) (83-terminating-program-example-10000.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: \at(i, AnyPrev) > i (83-terminating-program-example-10000.c:15:5)
  [Warning][Witness] invariant unconfirmed: i - j < \at(i, AnyPrev) - \at(j, AnyPrev) (83-terminating-program-example-10000.c:15:1)
  [Warning][Witness] invariant unconfirmed: i - j <= \at(i, AnyPrev) - \at(j, AnyPrev) (83-terminating-program-example-10000.c:15:1)
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 2
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
