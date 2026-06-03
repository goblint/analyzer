Ghost instrumentation for mutex, rwlock and spinlock lock/trylock with sem.lock.fail disabled.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 57-mutex-ghost-lockfail.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --disable sem.lock.fail --set colors never 57-mutex-ghost-lockfail.c
