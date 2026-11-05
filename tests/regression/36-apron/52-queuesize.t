`ana.apron.invariant.diff-box` test case from https://github.com/goblint/analyzer/pull/762.

Without diff-box:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --disable ana.base.invariant.enabled --set ana.relation.privatization mutex-meet --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.apron.domain polyhedra --enable ana.relation.invariant.one-var --disable ana.apron.invariant.diff-box 52-queuesize.c
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:67:5-67:31)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:68:5-68:38)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:69:5-69:31)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:70:5-70:38)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:71:5-71:45)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:15:3-15:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:16:3-16:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:17:3-17:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:18:3-18:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:19:3-19:43)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:26:3-26:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:27:3-27:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:28:3-28:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:29:3-29:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:30:3-30:43)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:36:3-36:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:37:3-37:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:38:3-38:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:39:3-39:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:40:3-40:43)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:47:3-47:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:48:3-48:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:49:3-49:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:50:3-50:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:51:3-51:43)
  [Warning][Deadcode] Function 'worker' has dead code:
    on line 58 (52-queuesize.c:58-58)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 53
    dead: 1
    total lines: 54
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (52-queuesize.c:56:10-56:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (52-queuesize.c:78:12-78:13)
  [Info][Witness] witness generation summary:
    location invariants: 8
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3

  $ yamlWitnessStrip < witness.yml | tee witness-disable-diff-box.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 15
          column: 3
          function: pop
        value: (long long )capacity >= (long long )free
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 15
          column: 3
          function: pop
        value: (long long )free >= 0LL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 15
          column: 3
          function: pop
        value: (long long )used + (long long )free == (long long )capacity
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 15
          column: 3
          function: pop
        value: 2147483647LL >= (long long )capacity
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 36
          column: 3
          function: push
        value: (long long )capacity >= (long long )free
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 36
          column: 3
          function: push
        value: (long long )free >= 0LL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 36
          column: 3
          function: push
        value: (long long )used + (long long )free == (long long )capacity
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 36
          column: 3
          function: push
        value: 2147483647LL >= (long long )capacity
        format: c_expression

With diff-box:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --disable ana.base.invariant.enabled --set ana.relation.privatization mutex-meet --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.apron.domain polyhedra --enable ana.relation.invariant.one-var --enable ana.apron.invariant.diff-box 52-queuesize.c
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:67:5-67:31)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:68:5-68:38)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:69:5-69:31)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:70:5-70:38)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:71:5-71:45)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:15:3-15:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:16:3-16:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:17:3-17:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:18:3-18:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:19:3-19:43)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:26:3-26:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:27:3-27:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:28:3-28:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:29:3-29:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:30:3-30:43)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:36:3-36:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:37:3-37:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:38:3-38:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:39:3-39:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:40:3-40:43)
  [Success][Assert] Assertion "free >= 0" will succeed (52-queuesize.c:47:3-47:29)
  [Success][Assert] Assertion "free <= capacity" will succeed (52-queuesize.c:48:3-48:36)
  [Success][Assert] Assertion "used >= 0" will succeed (52-queuesize.c:49:3-49:29)
  [Success][Assert] Assertion "used <= capacity" will succeed (52-queuesize.c:50:3-50:36)
  [Success][Assert] Assertion "used + free == capacity" will succeed (52-queuesize.c:51:3-51:43)
  [Warning][Deadcode] Function 'worker' has dead code:
    on line 58 (52-queuesize.c:58-58)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 53
    dead: 1
    total lines: 54
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (52-queuesize.c:56:10-56:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (52-queuesize.c:78:12-78:13)
  [Info][Witness] witness generation summary:
    location invariants: 4
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3

TODO: Should (long long )free >= 0LL somehow still remain, because it cannot be inferred by box alone?
  $ yamlWitnessStrip < witness.yml | tee witness-enable-diff-box.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 15
          column: 3
          function: pop
        value: (long long )capacity >= (long long )free
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 15
          column: 3
          function: pop
        value: (long long )used + (long long )free == (long long )capacity
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 36
          column: 3
          function: push
        value: (long long )capacity >= (long long )free
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 52-queuesize.c
          line: 36
          column: 3
          function: push
        value: (long long )used + (long long )free == (long long )capacity
        format: c_expression

Compare witnesses:

  $ yamlWitnessStripDiff witness-disable-diff-box.yml witness-enable-diff-box.yml
  # Left-only invariants:
  - invariant:
      type: location_invariant
      location:
        file_name: 52-queuesize.c
        line: 36
        column: 3
        function: push
      value: 2147483647LL >= (long long )capacity
      format: c_expression
  - invariant:
      type: location_invariant
      location:
        file_name: 52-queuesize.c
        line: 36
        column: 3
        function: push
      value: (long long )free >= 0LL
      format: c_expression
  - invariant:
      type: location_invariant
      location:
        file_name: 52-queuesize.c
        line: 15
        column: 3
        function: pop
      value: 2147483647LL >= (long long )capacity
      format: c_expression
  - invariant:
      type: location_invariant
      location:
        file_name: 52-queuesize.c
        line: 15
        column: 3
        function: pop
      value: (long long )free >= 0LL
      format: c_expression
  ---
