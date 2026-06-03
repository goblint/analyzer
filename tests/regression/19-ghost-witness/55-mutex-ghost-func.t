Ghost instrumentation where lock and unlock are wrapped in their own functions.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 55-mutex-ghost-func.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set ana.context.gas_value 3 --set colors never 55-mutex-ghost-func.c
