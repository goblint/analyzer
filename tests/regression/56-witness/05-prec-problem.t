  $ goblint --enable witness.yaml.enabled --enable ana.int.interval --set witness.yaml.entry-types '["precondition_loop_invariant"]' 05-prec-problem.c
  [Success][Assert] Assertion "y != z" will succeed (05-prec-problem.c:21:5-21:28)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 0
    total lines: 12
  [Info][Witness] witness generation summary:
    total generation entries: 10

Witness shouldn't contain two unsound precondition_loop_invariant-s with precondition `*ptr1 == 5 && *ptr2 == 5`,
and separately invariants `result == 0` and `result == 1`.
The sound invariant is `result == 1 || result == 0`.

  $ yamlWitnessStrip < witness.yml
  - entry_type: precondition_loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: $FILE_HASH
      line: 13
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
      file_hash: $FILE_HASH
      line: 13
      column: 4
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
      file_hash: $FILE_HASH
      line: 13
      column: 4
      function: foo
    loop_invariant:
      string: '*ptr1 == 5'
      type: assertion
      format: C
    precondition:
      string: '*ptr1 == 5 && *ptr2 == 5'
      type: assertion
      format: C
  - entry_type: precondition_loop_invariant
    location:
      file_name: 05-prec-problem.c
      file_hash: $FILE_HASH
      line: 7
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
      file_hash: $FILE_HASH
      line: 7
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
