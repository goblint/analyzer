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
