Both programs lose the last pointer to an allocation. A generic leak warning
also fires for reachable but unfreed memory, so check the memtrack verdict.

  $ check_memtrack () {
  >   goblint --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail \
  >     --enable ana.sv-comp.enabled --set ana.specification 'CHECK( init(main()), LTL(G valid-memtrack) )' \
  >     "$1" > output 2>&1 || { cat output; return 1; }
  >   grep -Eq '^SV-COMP result: (unknown|false(\(valid-memtrack\))?)$' output || { cat output; return 1; }
  > }
  $ check_memtrack 36-global-struct-summary-memtrack.c
  $ check_memtrack 37-global-struct-may-targets-memtrack.c
