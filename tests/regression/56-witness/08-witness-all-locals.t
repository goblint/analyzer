  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable witness.invariant.all-locals 08-witness-all-locals.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness generation summary:
    total generation entries: 3

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 08-witness-all-locals.c
      file_hash: $FILE_HASH
      line: 9
      column: 2
      function: main
    location_invariant:
      string: y == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 08-witness-all-locals.c
      file_hash: $FILE_HASH
      line: 9
      column: 2
      function: main
    location_invariant:
      string: x == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 08-witness-all-locals.c
      file_hash: $FILE_HASH
      line: 7
      column: 4
      function: main
    location_invariant:
      string: x == 5
      type: assertion
      format: C

Fewer entries are emitted if locals from nested block scopes are excluded:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable witness.invariant.all-locals 08-witness-all-locals.c
  [Warning] Disabling witness.invariant.all-locals implicitly enables cil.addNestedScopeAttr.
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness generation summary:
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 08-witness-all-locals.c
      file_hash: $FILE_HASH
      line: 9
      column: 2
      function: main
    location_invariant:
      string: x == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 08-witness-all-locals.c
      file_hash: $FILE_HASH
      line: 7
      column: 4
      function: main
    location_invariant:
      string: x == 5
      type: assertion
      format: C
