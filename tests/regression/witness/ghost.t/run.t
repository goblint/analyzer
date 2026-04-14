Ghost variables declared in a YAML witness are injected into the CIL file before analysis:

  $ goblint --enable justcil --set dbg.justcil-printer clean --set witness.yaml.validate ghost.yml ghost.c | grep "m_locked"
  int m_locked  = 0;
