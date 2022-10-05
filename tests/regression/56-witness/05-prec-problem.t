  $ goblint --enable witness.yaml.enabled --enable ana.int.interval 05-prec-problem.c
  [Success][Assert] Assertion "y != z" will succeed (05-prec-problem.c:20:5-20:28)
  Live lines: 12
  No lines with dead code found by solver.
  Total lines (logical LoC): 12
  [Info][Witness] witness generation summary:
    total: 15

Witness shouldn't contain two unsound precondition_loop_invariant-s with precondition `*ptr1 == 5 && *ptr2 == 5`,
and separately invariants `result == 0` and `result == 1`.
The sound invariant is `result == 1 || result == 0`.

  $ yamlWitnessStrip < witness.yml
  - entry_type: precondition_loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 12
      column: 4
      function: foo
    loop_invariant:
      string: result == 1 || result == 0
      type: assertion
      format: C
    precondition:
      string: '*ptr1 == 5 && *ptr2 == 5'
      type: assertion
      format: C
  - entry_type: precondition_loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 6
      column: 7
      function: foo
    loop_invariant:
      string: '*ptr2 == 5'
      type: assertion
      format: C
    precondition:
      string: '*ptr1 == 5 && *ptr2 == 5'
      type: assertion
      format: C
  - entry_type: precondition_loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 6
      column: 7
      function: foo
    loop_invariant:
      string: '*ptr1 == 5'
      type: assertion
      format: C
    precondition:
      string: '*ptr1 == 5 && *ptr2 == 5'
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 20
      column: 4
      function: main
    loop_invariant:
      string: z == 1
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 20
      column: 4
      function: main
    loop_invariant:
      string: y == 0
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 19
      column: 8
      function: main
    loop_invariant:
      string: y == 0
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 18
      column: 8
      function: main
    loop_invariant:
      string: five2 == 5
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 17
      column: 8
      function: main
    loop_invariant:
      string: five == 5
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 12
      column: 4
      function: foo
    loop_invariant:
      string: result <= 1
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 12
      column: 4
      function: foo
    loop_invariant:
      string: 0 <= result
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 6
      column: 7
      function: foo
    loop_invariant:
      string: '*ptr2 == 5 || *ptr2 == 5'
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: cf828ba93b8ed3289734bd38dd37f9e9fd808af7d303b61191df6737aa478928
      line: 6
      column: 7
      function: foo
    loop_invariant:
      string: '*ptr1 == 5'
      type: assertion
      format: C
