  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable witness.invariant.all-locals 08-witness-all-locals.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness generation summary:
    total generation entries: 3

TODO: check witness.yml content with yamlWitnessStrip

Fewer entries are emitted if locals from nested block scopes are excluded:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable witness.invariant.all-locals 08-witness-all-locals.c
  [Warning] Disabling witness.invariant.all-locals implicitly enables cil.addNestedScopeAttr.
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness generation summary:
    total generation entries: 2

TODO: check witness.yml content with yamlWitnessStrip
