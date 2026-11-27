TODO: Why not verified terminating?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable dbg.termination-bounds --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 82-terminating-program-example-3.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (82-terminating-program-example-3.c:15:12-15:20)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (82-terminating-program-example-3.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 5
    flow-insensitive invariants: 0
    loop transition invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: (long long )j + (long long )i == 4LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: 1 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: 2 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: i <= 3
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: j <= 2
        format: c_expression

TODO: Why not verified terminating?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable dbg.termination-bounds --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 82-terminating-program-example-3.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (82-terminating-program-example-3.c:15:12-15:20)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (82-terminating-program-example-3.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: 1 <= j (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: 2 <= i (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: i <= 3 (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: j <= 2 (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j + (long long )i == 4LL (82-terminating-program-example-3.c:15:5)
  [Info][Witness] witness validation summary:
    confirmed: 5
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 5

TODO: Why not verified terminating?
TODO: Why not invariants confirmed?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable dbg.termination-bounds --set witness.yaml.validate 82-terminating-program-example-3.witness-2.1.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 82-terminating-program-example-3.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (82-terminating-program-example-3.c:15:12-15:20)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (82-terminating-program-example-3.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Witness] invariant unconfirmed: \at(i, AnyPrev) > i (82-terminating-program-example-3.c:15:5)
  [Warning][Witness] invariant unconfirmed: i - j < \at(i, AnyPrev) - \at(j, AnyPrev) (82-terminating-program-example-3.c:15:1)
  [Warning][Witness] invariant unconfirmed: i - j <= \at(i, AnyPrev) - \at(j, AnyPrev) (82-terminating-program-example-3.c:15:1)
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 3
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3


Same with octagon:

TODO: Why not verified terminating?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable dbg.termination-bounds --enable ana.apron.invariant.diff-box --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 82-terminating-program-example-3.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (82-terminating-program-example-3.c:15:12-15:20)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (82-terminating-program-example-3.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 5
    flow-insensitive invariants: 0
    loop transition invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: (long long )j + (long long )i == 4LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: 1 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: 2 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: i <= 3
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-terminating-program-example-3.c
          line: 15
          column: 5
          function: main
        value: j <= 2
        format: c_expression

TODO: Why not verified terminating?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable dbg.termination-bounds --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 82-terminating-program-example-3.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (82-terminating-program-example-3.c:15:12-15:20)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (82-terminating-program-example-3.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: 1 <= j (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: 2 <= i (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: i <= 3 (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: j <= 2 (82-terminating-program-example-3.c:15:5)
  [Success][Witness] invariant confirmed: (long long )j + (long long )i == 4LL (82-terminating-program-example-3.c:15:5)
  [Info][Witness] witness validation summary:
    confirmed: 5
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 5

TODO: Why not verified terminating?
TODO: Why not invariants confirmed?

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain octagon --enable dbg.termination-bounds --set witness.yaml.validate 82-terminating-program-example-3.witness-2.1.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 82-terminating-program-example-3.c
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Warning][Termination] The program might not terminate! (Loop analysis) (82-terminating-program-example-3.c:15:12-15:20)
  [Warning][Integer > Overflow][CWE-190] Unsigned integer overflow in + (82-terminating-program-example-3.c:15:12-15:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Witness] invariant unconfirmed: \at(i, AnyPrev) > i (82-terminating-program-example-3.c:15:5)
  [Warning][Witness] invariant unconfirmed: i - j < \at(i, AnyPrev) - \at(j, AnyPrev) (82-terminating-program-example-3.c:15:1)
  [Warning][Witness] invariant unconfirmed: i - j <= \at(i, AnyPrev) - \at(j, AnyPrev) (82-terminating-program-example-3.c:15:1)
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 3
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
