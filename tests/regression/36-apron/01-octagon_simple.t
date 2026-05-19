Without diff-box:

  $ goblint --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --disable ana.apron.invariant.diff-box 01-octagon_simple.c
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:15:3-15:28)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:16:3-16:26)
  [Success][Assert] Assertion "N == 8" will succeed (01-octagon_simple.c:25:3-25:26)
  [Success][Assert] Assertion "X <= N" will succeed (01-octagon_simple.c:43:3-43:26)
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:54:3-54:30)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:55:3-55:26)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 22 (01-octagon_simple.c:22-22)
  [Warning][Deadcode] Function 'two' has dead code:
    on line 40 (01-octagon_simple.c:40-40)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 25
    dead: 2
    total lines: 27
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:9:6-9:11)
  [Warning][Deadcode][CWE-571] condition 'X == N' is always true (01-octagon_simple.c:18:6-18:12)
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:39:7-39:12)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (01-octagon_simple.c:45:10-45:11)
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
          line: 11
          column: 3
          function: main
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: (long long )X + (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: (long long )X + 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: (long long )X >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: 2147483647LL >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 11
          column: 3
          function: main
        value: 4294967294LL >= (long long )X + (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: (long long )X + (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: (long long )X + 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: (long long )X >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: 2147483647LL >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
          column: 3
          function: two
        value: 4294967294LL >= (long long )X + (long long )N
        format: c_expression

With diff-box:

  $ goblint --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --enable ana.apron.invariant.diff-box 01-octagon_simple.c
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:15:3-15:28)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:16:3-16:26)
  [Success][Assert] Assertion "N == 8" will succeed (01-octagon_simple.c:25:3-25:26)
  [Success][Assert] Assertion "X <= N" will succeed (01-octagon_simple.c:43:3-43:26)
  [Success][Assert] Assertion "X - N == 0" will succeed (01-octagon_simple.c:54:3-54:30)
  [Success][Assert] Assertion "X == N" will succeed (01-octagon_simple.c:55:3-55:26)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 22 (01-octagon_simple.c:22-22)
  [Warning][Deadcode] Function 'two' has dead code:
    on line 40 (01-octagon_simple.c:40-40)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 25
    dead: 2
    total lines: 27
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:9:6-9:11)
  [Warning][Deadcode][CWE-571] condition 'X == N' is always true (01-octagon_simple.c:18:6-18:12)
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (01-octagon_simple.c:39:7-39:12)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (01-octagon_simple.c:45:10-45:11)
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
          line: 11
          column: 3
          function: main
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 01-octagon_simple.c
          line: 45
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
        line: 45
        column: 3
        function: two
      value: 4294967294LL >= (long long )X + (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 45
        column: 3
        function: two
      value: 2147483647LL >= (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 45
        column: 3
        function: two
      value: 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 45
        column: 3
        function: two
      value: (long long )X >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 45
        column: 3
        function: two
      value: (long long )X + 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 45
        column: 3
        function: two
      value: (long long )X + (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 45
        column: 3
        function: two
      value: (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: 4294967294LL >= (long long )X + (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: 2147483647LL >= (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: (long long )X >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: (long long )X + 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: (long long )X + (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 01-octagon_simple.c
        line: 11
        column: 3
        function: main
      value: (long long )N >= 0LL
      format: c_expression
  ---
