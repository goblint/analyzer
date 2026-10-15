Regression test for issue #2159: the affine-equality Apron invariant exporter
must not negate the constant term. Before the fix the exported witness contained
the self-contradictory conjunct "x + 1 == z" alongside "x == z + 1".

  $ goblint --set ana.activated[+] affeq --set ana.activated[+] apron --set ana.autotune.enabled true --set ana.autotune.activated '["loopUnrollHeuristic"]' --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled 08-apron-invariant-constant-sign.c
  [Info] unrolling loop at 08-apron-invariant-constant-sign.c:8:3-9:14 with factor 4
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on line 10 (08-apron-invariant-constant-sign.c:10-10)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 1
    total lines: 4
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (08-apron-invariant-constant-sign.c:8:10-8:11)
  [Info][Witness] witness generation summary:
    location invariants: 1
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 08-apron-invariant-constant-sign.c
          line: 9
          column: 5
          function: main
        value: (((((-0x7FFFFFFF-1) <= x && x <= 2147483647LL) && 4294967295LL >= (long
          long )z + x) && ((long long )z + x) + 4294967295LL >= 0LL) && x == (long long
          )z + 1LL) || ((((2147483648LL >= (long long )z + x && x + 2147483646LL >=
          (long long )z) && ((long long )z + x) + 2147483647LL >= 0LL) && (long long
          )z + 2147483649LL >= x) && x == 1LL)
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 08-apron-invariant-constant-sign.c
          line: 8
          column: 3
          function: main
        value: (((((-0x7FFFFFFF-1) <= x && x <= 2147483647LL) && 4294967295LL >= (long
          long )z + x) && ((long long )z + x) + 4294967295LL >= 0LL) && x == (long long
          )z + 1LL) || ((((2147483648LL >= (long long )z + x && x + 2147483646LL >=
          (long long )z) && ((long long )z + x) + 2147483647LL >= 0LL) && (long long
          )z + 2147483649LL >= x) && x == 1LL)
        format: c_expression
