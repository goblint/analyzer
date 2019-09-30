# Goblint for SV-COMP

## Run Goblint in SV-COMP mode
Command:
```
./goblint --enable ana.sv-comp --enable exp.uncilwitness --enable ana.int.interval ./tests/sv-comp/basic/if_mod_true-unreach-call.c
```

There's a bunch of very simple files to test with in `./tests/sv-comp/` with the expected results in the filename (old SV-COMP task definition format).

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