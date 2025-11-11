Without diff-box:

  $ goblint --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --disable ana.apron.invariant.diff-box 03-octagon_simplest.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (03-octagon_simplest.c:9:6-9:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 17
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml | tee witness-disable-diff-box.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )N >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )N >= (long long )Y
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )X + (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )X + 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )X >= (long long )Y
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )X >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )Y + (long long )N >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )Y + (long long )X >= 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )Y + 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )Y + 2147483647LL >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )Y == 0LL
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: 2147483647LL >= (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: 2147483647LL >= (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: 2147483647LL >= (long long )Y + (long long )N
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: 2147483647LL >= (long long )Y + (long long )X
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: 4294967294LL >= (long long )X + (long long )N
        format: c_expression

With diff-box:

  $ goblint --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --enable ana.apron.invariant.diff-box 03-octagon_simplest.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8
  [Warning][Deadcode][CWE-570] condition 'N < 0' is always false (03-octagon_simplest.c:9:6-9:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml | tee witness-enable-diff-box.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 03-octagon_simplest.c
          line: 11
          column: 3
          function: main
        value: (long long )N >= (long long )X
        format: c_expression

Compare witnesses:

  $ yamlWitnessStripDiff witness-disable-diff-box.yml witness-enable-diff-box.yml
  # Left-only invariants:
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: 4294967294LL >= (long long )X + (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: 2147483647LL >= (long long )Y + (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: 2147483647LL >= (long long )Y + (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: 2147483647LL >= (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )Y == 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )Y + 2147483647LL >= (long long )X
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )Y + 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )Y + (long long )X >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )Y + (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )X >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )X >= (long long )Y
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )X + 2147483647LL >= (long long )N
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )X + (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )N >= 0LL
      format: c_expression
  - invariant:
      type: loop_invariant
      location:
        file_name: 03-octagon_simplest.c
        line: 11
        column: 3
        function: main
      value: (long long )N >= (long long )Y
      format: c_expression
  ---
