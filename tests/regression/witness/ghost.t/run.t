Ghost variables declared in a YAML witness are injected into the CIL file before analysis:

  $ goblint --enable justcil --set dbg.justcil-printer clean --set witness.yaml.validate ghost.yml ghost.c | grep -E "m_locked"
  int m_locked  =    0;

Ghost updates from a YAML witness are inserted after the matching statement, wrapped in atomic blocks:

  $ goblint --enable justcil --set dbg.justcil-printer clean --set witness.yaml.validate ghost-update.yml ghost-update.c | grep -E "atomic_begin|x = 1|g_var = 1|atomic_end|x = 2"
    __VERIFIER_atomic_begin();
    x = 1;
    g_var = 1;
    __VERIFIER_atomic_end();
    x = 2;

A ghost update at a line with no matching instruction produces a warning:

  $ goblint --enable justcil --set dbg.justcil-printer clean --set witness.yaml.validate ghost-update-unplaced.yml ghost-update-unplaced.c 2>&1 | grep "could not be placed"
  [Warning][Witness] ghost update at ghost-update-unplaced.c:99 could not be placed: no matching instruction found

An unplaced ghost update prevents successful validation:

  $ goblint --set witness.yaml.validate ghost-update-unplaced.yml ghost-update-unplaced.c 2>&1 | grep -E "could not be placed|cannot be successful"
  [Warning][Witness] ghost update at ghost-update-unplaced.c:99 could not be placed: no matching instruction found
  [Warning][Witness] validation result cannot be successful: some ghost updates could not be placed
