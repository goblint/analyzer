Ghost instrumentation for pthread_mutex_trylock with sem.lock.fail enabled.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 61-mutex-ghost-trylock.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set sem.lock.fail true --set colors never 61-mutex-ghost-trylock.c
