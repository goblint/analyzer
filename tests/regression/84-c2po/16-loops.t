  $ goblint --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false --enable witness.yaml.enabled --disable ana.base.invariant.enabled 16-loops.c
  [Success][Assert] Assertion "(unsigned long )z == (unsigned long )(x + -1)" will succeed (16-loops.c:26:3-26:31)
  [Warning][Assert] Assertion "y == *x2" is unknown. (16-loops.c:27:3-27:28)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Info][Witness] witness generation summary:
    location invariants: 22
    loop invariants: 2
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 13
          column: 3
          function: main
        value: x2 == x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 15
          column: 3
          function: main
        value: x2 == x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 16
          column: 3
          function: main
        value: x2 == x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 16
          column: 3
          function: main
        value: y == *x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 19
          column: 5
          function: main
        value: '*x == *(z + 1)'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 19
          column: 5
          function: main
        value: z == x + -1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 20
          column: 5
          function: main
        value: x != z
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 20
          column: 5
          function: main
        value: x2 != z
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 20
          column: 5
          function: main
        value: z != *x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 21
          column: 5
          function: main
        value: x != z
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 21
          column: 5
          function: main
        value: x2 != z
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 21
          column: 5
          function: main
        value: z != *(x + -1)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 22
          column: 5
          function: main
        value: '*(x + -1) == *z'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 22
          column: 5
          function: main
        value: z == x + -1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 23
          column: 5
          function: main
        value: '*(x + -1) == *z'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 23
          column: 5
          function: main
        value: z == x + -1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 26
          column: 3
          function: main
        value: '*x == *(z + 1)'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 26
          column: 3
          function: main
        value: z == x + -1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 27
          column: 3
          function: main
        value: '*x == *(z + 1)'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 27
          column: 3
          function: main
        value: z == x + -1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 29
          column: 3
          function: main
        value: '*x == *(z + 1)'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 16-loops.c
          line: 29
          column: 3
          function: main
        value: z == x + -1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 16-loops.c
          line: 18
          column: 3
          function: main
        value: '*x == *(z + 1)'
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 16-loops.c
          line: 18
          column: 3
          function: main
        value: z == x + -1
        format: c_expression
