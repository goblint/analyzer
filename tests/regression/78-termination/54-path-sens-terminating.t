  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --set ana.activated[+] expsplit --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 54-path-sens-terminating.c | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
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
          file_name: 54-path-sens-terminating.c
          line: 14
          column: 3
          function: main
        value: ((((0 <= c && 0 <= x) && x <= 20) && mode == 0) && (long long )x == (long
          long )c) || ((((-99 <= x && 1 <= c) && x <= -80) && mode == 1) && (long long
          )x + 100LL == (long long )c)
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 54-path-sens-terminating.c
          line: 14
          column: 3
          function: main
        value: c <= 20
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 54-path-sens-terminating.c
          line: 14
          column: 3
          function: main
        value: (long long )c >= (long long )\at(c, AnyPrev) + 1LL
        format: ext_c_expression

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --set ana.activated[+] expsplit --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 54-path-sens-terminating.c 
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
  [Success][Witness] invariant confirmed: c <= 20 (54-path-sens-terminating.c:14:3)
  [Success][Witness] invariant confirmed: ((((0 <= c && 0 <= x) && x <= 20) && mode == 0) && (long long )x == (long long )c) || ((((-99 <= x && 1 <= c) && x <= -80) && mode == 1) && (long long )x + 100LL == (long long )c) (54-path-sens-terminating.c:14:3)
  [Success][Witness] invariant confirmed: (long long )c >= (long long )\at(c, AnyPrev) + 1LL (54-path-sens-terminating.c:14:3)
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

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --set ana.activated[+] expsplit --enable ana.int.interval --set ana.apron.domain polyhedra --enable witness.yaml.enabled --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 54-path-sens-terminating.c --set exp.unrolling-factor 1 | sed -r 's/term[0-9]+_[0-9]+_id[0-9]+#[0-9]+/$TERM/'
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info] unrolling loop at 54-path-sens-terminating.c:14:3-18:3 with factor 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
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
          file_name: 54-path-sens-terminating.c
          line: 14
          column: 3
          function: main
        value: (c <= 20 && (((((2 <= c && 2 <= x) && x <= 20) && mode == 0) && (long
          long )x == (long long )c) || ((((-99 <= x && 1 <= c) && x <= -80) && mode
          == 1) && (long long )x + 100LL == (long long )c))) || ((c == 0 && x == 0)
          && mode == 0)
        format: c_expression
    - invariant:
        type: loop_transition_invariant
        location:
          file_name: 54-path-sens-terminating.c
          line: 14
          column: 3
          function: main
        value: (long long )c >= (long long )\at(c, AnyPrev) + 1LL
        format: ext_c_expression

  $ goblint --set ana.activated[+] termination --set ana.activated[+] apron --set ana.activated[+] expsplit --enable ana.int.interval --set ana.apron.domain polyhedra --set witness.yaml.validate witness.yml --set witness.yaml.invariant-types "['loop_invariant', 'loop_transition_invariant']" 54-path-sens-terminating.c --set exp.unrolling-factor 1
  [Warning] termination analysis implicitly activates threadflag analysis and set sem.int.signed_overflow to assume_none
  [Info] unrolling loop at 54-path-sens-terminating.c:14:3-18:3 with factor 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
  [Success][Witness] invariant confirmed: (c <= 20 && (((((2 <= c && 2 <= x) && x <= 20) && mode == 0) && (long long )x == (long long )c) || ((((-99 <= x && 1 <= c) && x <= -80) && mode == 1) && (long long )x + 100LL == (long long )c))) || ((c == 0 && x == 0) && mode == 0) (54-path-sens-terminating.c:14:3)
  [Error][Witness] invariant refuted: (long long )c >= (long long )\at(c, AnyPrev) + 1LL (54-path-sens-terminating.c:14:3)
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 1
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2
