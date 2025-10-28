  $ goblint --disable ana.dead-code.lines --disable warn.race --enable warn.deterministic --disable warn.behavior --set witness.yaml.invariant-types '["location_invariant"]' --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --enable witness.yaml.enabled --disable witness.invariant.other --disable witness.invariant.loop-head 96-witness-mm-escape2.c --set witness.yaml.path 96-witness-mm-escape2.yml
  [Info][Witness] witness generation summary:
    location invariants: 4
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ goblint --disable ana.dead-code.lines --disable warn.race --enable warn.deterministic --disable warn.behavior --set witness.yaml.invariant-types '["location_invariant"]' --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.validate 96-witness-mm-escape2.yml 96-witness-mm-escape2.c
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: (unsigned long )arg == 0UL (96-witness-mm-escape2.c:8:5)
  [Success][Witness] invariant confirmed: -128 <= g (96-witness-mm-escape2.c:22:1)
  [Success][Witness] invariant confirmed: g != 0 (96-witness-mm-escape2.c:22:1)
  [Success][Witness] invariant confirmed: g <= 127 (96-witness-mm-escape2.c:22:1)
