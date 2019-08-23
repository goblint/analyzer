
## Run Goblint in SV-COMP mode
Minimal command:
```
./goblint --enable ana.sv-comp --enable exp.minwitness --enable ana.int.interval ./tests/sv-comp/basic/if_mod_true-unreach-call.c
```

My command, just in case:
```
./goblint --enable ana.sv-comp --enable exp.minwitness --enable dbg.debug --set ana.activated[+] "'var_eq'" --enable ana.int.interval --set solver "'topdown'" --html ./tests/sv-comp/basic/if_mod_true-unreach-call.c
```

## Run Ultimate to verify witness
From: https://github.com/sosy-lab/sv-witnesses/#validating-a-violation-witness-with-ultimate-automizer.

`PropertyUnreachCall.prp` file:
```
CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )
```

Command:
```
./Ultimate.py --spec PropertyUnreachCall.prp --architecture 32bit --file ../goblint/tests/sv-comp/basic/if_mod_true-unreach-call.c --validate ../goblint/witness.graphml
```