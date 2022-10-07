  $ goblint --enable witness.yaml.enabled --disable witness.invariant.accessed --set pre.cppflags[+] -DGOBLINT_NO_PTHREAD_ONCE 28-base-mutex-macos.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total: 2
  [Info][Witness] witness generation summary:
    total: 0

There should be no invariants about __sig.
Base analysis should hide mutex contents.

  $ yamlWitnessStrip < witness.yml
  []
