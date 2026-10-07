Ghost variables declared in a YAML witness are injected into the CIL file before analysis:

  $ goblint --enable justcil --set dbg.justcil-printer clean --set witness.yaml.validate ghost.yml ghost.c | grep -E "m_locked"
  int m_locked  =    0;

For assignments with ghost updates whose right-hand side reads a global or an
address-taken variable, or contains a memory dereference, the right-hand side
is evaluated into a fresh local before the atomic block. The assignment from
that local and the ghost update are inserted together inside the atomic block.
Other assignments remain entirely inside the atomic block:

  $ goblint --enable justcil --set dbg.justcil-printer clean --set witness.yaml.validate ghost-update.yml ghost-update.c | grep -E "__goblint_ghost_rhs[0-9]+ = (y|\\*p)|atomic_instrument_begin|x = __goblint_ghost_rhs[0-9]+|g_var = [123]|atomic_instrument_end|x = 2" | sed -E 's/__goblint_ghost_rhs[0-9]+/__goblint_ghost_rhsN/g'
    __goblint_ghost_rhsN = y;
    __VERIFIER_atomic_instrument_begin();
    x = __goblint_ghost_rhsN;
    g_var = 1;
    __VERIFIER_atomic_instrument_end();
    __VERIFIER_atomic_instrument_begin();
    x = 2;
    g_var = 2;
    __VERIFIER_atomic_instrument_end();
    __goblint_ghost_rhsN = *p;
    __VERIFIER_atomic_instrument_begin();
    x = __goblint_ghost_rhsN;
    g_var = 3;
    __VERIFIER_atomic_instrument_end();

A ghost update at a line with no matching instruction produces a warning:

  $ goblint --set witness.yaml.validate ghost-update-unplaced.yml ghost-update-unplaced.c 2>&1 | grep "no matching instruction found"
  [Warning][Witness] ghost update at ghost-update-unplaced.c:99:3 could not be placed: no matching instruction found

An unplaced ghost update prevents successful validation:

  $ goblint --set witness.yaml.validate ghost-update-unplaced.yml ghost-update-unplaced.c 2>&1 | grep -E "could not be placed|cannot be successful"
  [Warning][Witness] ghost update at ghost-update-unplaced.c:99:3 could not be placed: no matching instruction found
  [Warning][Witness] validation result cannot be successful: some ghost updates could not be placed
