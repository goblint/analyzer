  $ goblint --conf svcomp25.json --disable ana.autotune.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"} nec11.c --enable exp.arg.enabled --enable ana.wp --set witness.yaml.entry-types[+] violation_sequence --enable witness.yaml.sv-comp-true-only --enable witness.invariant.other 
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow (nec11.c:28:7-28:12)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16
  SV-COMP result: unknown

  $ yamlWitnessStrip < witness.yml
  - entry_type: violation_sequence
    content:
    - segment:
      - waypoint:
          type: assumption
          location:
            file_name: nec11.c
            line: 16
            column: 4
            function: main
          action: follow
          constraint:
            value: "1"
            format: c_expression
    - segment:
      - waypoint:
          type: assumption
          location:
            file_name: nec11.c
            line: 17
            column: 4
            function: main
          action: follow
          constraint:
            value: "1"
            format: c_expression
    - segment:
      - waypoint:
          type: branching
          location:
            file_name: nec11.c
            line: 21
            column: 4
            function: main
          action: follow
          constraint:
            value: "false"
            format: c_expression
    - segment:
      - waypoint:
          type: assumption
          location:
            file_name: nec11.c
            line: 30
            column: 4
            function: main
          action: follow
          constraint:
            value: "1"
            format: c_expression
    - segment:
      - waypoint:
          type: branching
          location:
            file_name: nec11.c
            line: 6
            column: 3
            function: __VERIFIER_assert
          action: follow
          constraint:
            value: "true"
            format: c_expression
    - segment:
      - waypoint:
          type: target
          location:
            file_name: nec11.c
            line: 7
            column: 13
            function: __VERIFIER_assert
          action: follow
