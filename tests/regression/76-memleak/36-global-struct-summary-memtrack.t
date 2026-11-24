Overwriting root.p loses the first allocation even though both live objects
share a summary address.

  $ goblint --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail \
  >   --enable ana.sv-comp.enabled --set ana.specification 'CHECK( init(main()), LTL(G valid-memtrack) )' \
  >   --enable witness.yaml.enabled 36-global-struct-summary-memtrack.c
  [Info] Specification: CHECK( init(main()), LTL(G valid-memtrack) ) -> enabling soundness analyses: "memLeak"
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G valid-memtrack) )
  [Warning][Behavior > Undefined > MemoryLeak][CWE-401] There is unreachable allocated heap memory at program exit. A memory leak might occur for the alloc vars (alloc@sid:13@tid:[main]) (36-global-struct-summary-memtrack.c:14:10-14:11)
  [Warning][Behavior > Undefined > MemoryLeak][CWE-401] Memory leak detected for heap variables (36-global-struct-summary-memtrack.c:14:10-14:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  SV-COMP result: unknown
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    loop transition invariants: 0
    total generation entries: 1
  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 36-global-struct-summary-memtrack.c
          line: 11
          column: 3
          function: main
        value: (unsigned long )root.p == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 36-global-struct-summary-memtrack.c
          line: 12
          column: 3
          function: main
        value: (unsigned long )root.p == 0UL
        format: c_expression
