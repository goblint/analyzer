The invariant at the dead location is parsed and reported as invalid:

  $ goblint --set witness.yaml.validate 82-validate-dead-parse-error.yml 82-validate-dead-parse-error.c
  [Warning][Deadcode] Function 'main' has dead code:
    on line 7 (82-validate-dead-parse-error.c:7-7)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 1
    total lines: 5
  [Warning][Deadcode][CWE-570] condition 'x' is always false (82-validate-dead-parse-error.c:6:7-6:8)
  [Error][Witness] CIL couldn't parse invariant: y == 0 (82-validate-dead-parse-error.c:7:5)
  [Info][Witness] invariant has undefined variables or side effects: y == 0 (82-validate-dead-parse-error.c:7:5)
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 1
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  82-validate-dead-parse-error.c:7: Error: Cannot resolve variable y.
