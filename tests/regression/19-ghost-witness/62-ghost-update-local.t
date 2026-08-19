Ghost assignment right-hand sides may refer to variables local to the function
containing the update location.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 62-ghost-update-local.yml --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 62-ghost-update-local.c 2>&1 | grep -E 'ghost update value parse failed|invariant (confirmed|unconfirmed)'
  [Success][Witness] invariant confirmed: ghost == 42 (62-ghost-update-local.c:6:3)
