Violation witness for a correct program can be refuted by proving the program correct and returning `true`:

  $ goblint --enable ana.sv-comp.enabled --set witness.yaml.entry-types[+] violation_sequence --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" correct.c --set witness.yaml.validate correct.yml
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (correct.c:1:1-1:20)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 1 (1 in uncalled functions)
    total lines: 3
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
  SV-COMP result: true

If a correct progtam cannot be proven correct, return `unknown` for the violation witness:

  $ goblint --set ana.activated[-] expRelation --enable ana.sv-comp.functions --enable ana.sv-comp.enabled --set witness.yaml.entry-types[+] violation_sequence --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" correct-hard.c --set witness.yaml.validate correct-hard.yml
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
  SV-COMP result: unknown

Violation witness for an incorrect program cannot be proven correct, so return `unknown`:

  $ goblint --enable ana.sv-comp.enabled --set witness.yaml.entry-types[+] violation_sequence --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" incorrect.c --set witness.yaml.validate incorrect.yml
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
  SV-COMP result: unknown

  $ goblint --enable ana.sv-comp.enabled --set ana.activated[+] violationSequence --set witness.yaml.entry-types[+] violation_sequence --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" incorrect-follow-branching.c --set witness.yaml.validate incorrect-follow-branching.yml --set witness.yaml.observe incorrect-follow-branching.yml
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
  SV-COMP result: unknown
