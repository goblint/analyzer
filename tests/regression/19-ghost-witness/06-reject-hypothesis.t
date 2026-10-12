Run witness validation with `phaseGhost` on a variant of `05` where `ghost_a`
is updated from both the spawned thread and `main`. Check that we do not claim that it is 1 in the main thread
which we would if we blindly trusted it to be a phase ghost.

  $ goblint  --disable warn.race --disable warn.deadcode --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 06-reject-hypothesis.yml --set ana.activated[+] phaseGhost --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet --set colors never 06-reject-hypothesis.c
  [Warning][Witness] phaseGhost: global ghost_a is accessed by multiple unique threads: {[main, fun@06-reject-hypothesis.c:27:5-27:51], [main]}
  [Warning][Witness] invariant unconfirmed: ghost_a == 1 (06-reject-hypothesis.c:40:5)
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
