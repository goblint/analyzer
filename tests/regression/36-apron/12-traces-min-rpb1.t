  $ goblint --enable witness.yaml.enabled --disable witness.invariant.other --disable ana.base.invariant.enabled --set ana.apron.privatization mutex-meet --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.apron.domain polyhedra 12-traces-min-rpb1.c
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:16:3-16:26)
  [Warning][Assert] Assertion "g == h" is unknown. (12-traces-min-rpb1.c:27:3-27:26)
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:29:3-29:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total: 18
  [Warning][Race] Memory location g@12-traces-min-rpb1.c:7:5-7:10 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]},
              lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110) (12-traces-min-rpb1.c:14:3-14:8)
    read with [mhp:{tid=[main]; created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110) (12-traces-min-rpb1.c:27:3-27:26)
  [Warning][Race] Memory location h@12-traces-min-rpb1.c:8:5-8:10 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}, lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110) (12-traces-min-rpb1.c:15:3-15:8)
    read with [mhp:{tid=[main]; created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110) (12-traces-min-rpb1.c:27:3-27:26)
  [Info][Witness] witness generation summary:
    total: 3
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $STRIPPED_FILE_HASH
      line: 29
      column: 2
      function: main
    loop_invariant:
      string: (0LL - (long long )g) + (long long )h == 0LL
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $STRIPPED_FILE_HASH
      line: 19
      column: 2
      function: t_fun
    loop_invariant:
      string: (0LL - (long long )g) + (long long )h == 0LL
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $STRIPPED_FILE_HASH
      line: 14
      column: 2
      function: t_fun
    loop_invariant:
      string: (0LL - (long long )g) + (long long )h == 0LL
      type: assertion
      format: C
