Ghost instrumentation across helper functions called with different mutex arguments.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 54-mutex-ghost-munge.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set colors never 54-mutex-ghost-munge.c
