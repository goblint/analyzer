Ghost variables declared in a YAML witness are injected into the CIL file before analysis:

  $ goblint --set witness.yaml.validate ghost.yml ghost.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Warning][Witness] cannot validate entry of type ghost_instrumentation
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 1
    disabled: 0
    total validation entries: 1
