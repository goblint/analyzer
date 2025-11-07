  $ goblint --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false --enable witness.yaml.enabled --disable ana.base.invariant.enabled 16-loops.c
  [Success][Assert] Assertion "(unsigned long )z == (unsigned long )(x + -1)" will succeed (16-loops.c:26:3-26:31)
  [Warning][Assert] Assertion "y == *x2" is unknown. (16-loops.c:27:3-27:28)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content: []
