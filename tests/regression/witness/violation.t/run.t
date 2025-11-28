  $ goblint --conf svcomp25.json --disable ana.autotune.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"} callfpointer.c --enable exp.arg.stack --enable exp.arg.enabled --set exp.arg.dot.path arg.dot --set exp.arg.dot.node-label empty --enable ana.wp  --set witness.yaml.entry-types[+] violation_sequence --enable witness.yaml.sv-comp-true-only --enable witness.invariant.other 
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'f' has dead code:
    on line 8 (callfpointer.c:8-8)
  [Warning][Deadcode] Function 'h' has dead code:
    on line 16 (callfpointer.c:16-16)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 20 (callfpointer.c:20-20)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 3
    total lines: 11
  [Warning][Deadcode][CWE-571] condition 'i == 1' is always true (callfpointer.c:11:5-11:9)
  [Info]   fun316main(12)[7] =[Entry main]=> s11(12)[7]
  [Info]   s11(12)[7] =[InlineEntry '(& h)']=> fun309f(18)[16]@s11(12)[7]
  [Info]   fun309f(18)[16]@s11(12)[7] =[Entry f]=> s3(18)[16]@s11(12)[7]
  [Info]   s3(18)[16]@s11(12)[7] =[InlineEntry '(1)']=> fun313h(24)[21]@s3(18)[16]@s11(12)[7]
  [Info]   fun313h(24)[21]@s3(18)[16]@s11(12)[7] =[Entry h]=> s5(24)[21]@s3(18)[16]@s11(12)[7]
  [Info]   s5(24)[21]@s3(18)[16]@s11(12)[7] =[Test (i == 1,true)]=> s7(24)[21]@s3(18)[16]@s11(12)[7]
  SV-COMP result: unknown

  $ graph-easy --as=boxart arg.dot
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ Entry main
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ InlineEntry '(& h)'
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ Entry f
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ InlineEntry '(1)'
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ Entry h
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ Test (i == 1,true)
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ InlineEntry '()'
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘
    │
    │ Entry reach_error
    ▼
  ┌──────────────────────┐
  │          _           │
  └──────────────────────┘

  $ yamlWitnessStrip < witness.yml
  - entry_type: violation_sequence
    content:
    - segment:
      - waypoint:
          type: assumption
          location:
            file_name: callfpointer.c
            line: 18
            column: 2
            function: main
          action: follow
          constraint:
            value: "1"
            format: c_expression
    - segment:
      - waypoint:
          type: assumption
          location:
            file_name: callfpointer.c
            line: 7
            column: 2
            function: f
          action: follow
          constraint:
            value: "1"
            format: c_expression
    - segment:
      - waypoint:
          type: branching
          location:
            file_name: callfpointer.c
            line: 11
            column: 2
            function: h
          action: follow
          constraint:
            value: "true"
            format: c_expression
    - segment:
      - waypoint:
          type: target
          location:
            file_name: callfpointer.c
            line: 12
            column: 11
            function: h
          action: follow
