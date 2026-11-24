At exit, root.p can point to either allocation, but only one of them remains
reachable on each execution.

  $ goblint --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail \
  >   --enable ana.sv-comp.enabled --set ana.specification 'CHECK( init(main()), LTL(G valid-memtrack) )' \
  >   --enable witness.yaml.enabled 37-global-struct-may-targets-memtrack.c
  [Info] Specification: CHECK( init(main()), LTL(G valid-memtrack) ) -> enabling soundness analyses: "memLeak"
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G valid-memtrack) )
  [Warning][Behavior > Undefined > MemoryLeak][CWE-401] There is unreachable allocated heap memory at program exit. A memory leak might occur for the alloc vars (alloc@sid:15@tid:[main](#0)), (alloc@sid:13@tid:[main](#0)) (37-global-struct-may-targets-memtrack.c:10:10-10:11)
  [Warning][Behavior > Undefined > MemoryLeak][CWE-401] Memory leak detected for heap variables (37-global-struct-may-targets-memtrack.c:10:10-10:11)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  SV-COMP result: unknown
  [Info][Witness] witness generation summary:
    location invariants: 3
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
          file_name: 37-global-struct-may-targets-memtrack.c
          line: 7
          column: 3
          function: main
        value: (unsigned long )root.p == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 37-global-struct-may-targets-memtrack.c
          line: 8
          column: 3
          function: main
        value: (unsigned long )root.p == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 37-global-struct-may-targets-memtrack.c
          line: 9
          column: 3
          function: main
        value: (unsigned long )root.p == 0UL
        format: c_expression
