  $ goblint --set ana.activated[+] var_eq --enable witness.yaml.enabled --enable witness.invariant.other 71-var_eq-str-invariant.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 0
    total lines: 3
  [Info][Witness] witness generation summary:
    location invariants: 1
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

Should not contain invariant with string literal equality:

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-str-invariant.c
          line: 4
          column: 3
          function: main
        value: '"foobar" == text'
        format: c_expression
