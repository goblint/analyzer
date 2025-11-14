Without diff-box:

  $ goblint --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --disable ana.apron.invariant.diff-box 01-octagon_simple.c
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:14:3-14:28)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:15:3-15:26)
  [Success][Assert] Assertion "N == 8" will succeed (01-octagon_simple.c:24:3-24:26)
  [Success][Assert] Assertion "X <= N" will succeed (01-octagon_simple.c:42:3-42:26)
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:53:3-53:30)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:54:3-54:26)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 21 (01-octagon_simple.c:21-21)
  [Warning][Deadcode] Function 'two' has dead code:
    on line 39 (01-octagon_simple.c:39-39)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 25
    dead: 2
    total lines: 27
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:8:6-8:11)
  [Warning][Deadcode][CWE-571] condition 'X == N' is always true (01-octagon_simple.c:17:6-17:12)
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:38:7-38:12)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (01-octagon_simple.c:44:10-44:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 16
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml | tee witness-disable-diff-box.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: (long long )X + (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: (long long )X + 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: (long long )X >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: 2147483647LL >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: 4294967294LL >= (long long )X + (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: (long long )X + (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: (long long )X + 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: (long long )X >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: 2147483647LL >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: 4294967294LL >= (long long )X + (long long )N
        format: c_expression

With diff-box:

  $ goblint --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --enable ana.apron.invariant.diff-box 01-octagon_simple.c
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:14:3-14:28)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:15:3-15:26)
  [Success][Assert] Assertion "N == 8" will succeed (01-octagon_simple.c:24:3-24:26)
  [Success][Assert] Assertion "X <= N" will succeed (01-octagon_simple.c:42:3-42:26)
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:53:3-53:30)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:54:3-54:26)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 21 (01-octagon_simple.c:21-21)
  [Warning][Deadcode] Function 'two' has dead code:
    on line 39 (01-octagon_simple.c:39-39)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 25
    dead: 2
    total lines: 27
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:8:6-8:11)
  [Warning][Deadcode][CWE-571] condition 'X == N' is always true (01-octagon_simple.c:17:6-17:12)
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:38:7-38:12)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (01-octagon_simple.c:44:10-44:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 2
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml | tee witness-enable-diff-box.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 10
          column: 3
          function: main
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 44
          column: 3
          function: two
        value: (long long )N >= (long long )X
        format: c_expression

Compare witnesses:

  $ yamlWitnessStripDiff witness-disable-diff-box.yml witness-enable-diff-box.yml
  # Left-only invariants:
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: 4294967294LL >= (long long )X + (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: 2147483647LL >= (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: (long long )X >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: (long long )X + 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: (long long )X + (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 44
        column: 3
        function: two
      value: (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: 4294967294LL >= (long long )X + (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: 2147483647LL >= (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: (long long )X >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: (long long )X + 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: (long long )X + (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 10
        column: 3
        function: main
      value: (long long )N >= 0LL
      format: c_expression
  ---
