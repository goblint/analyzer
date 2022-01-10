# Goblint for SV-COMP
All the SV-COMP configuration is in `conf/svcomp21.json`.

## Run Goblint in SV-COMP mode
### ReachSafety
```
./goblint --conf conf/svcomp21.json --set ana.specification ../sv-benchmarks/c/properties/unreach-call.prp ../sv-benchmarks/c/DIR/FILE.i
```

### NoDataRace
```
./goblint --conf conf/svcomp21.json --set ana.specification ../sv-benchmarks/c/properties/no-data-race.prp ../sv-benchmarks/c/DIR/FILE.i
```

## Run Goblint under BenchExec
This requires setting up a bunch of XMLs and running a few things in the right places.
Also all paths must be properly configured.

There's three shell scripts in `sv-comp/benchexec/my-bench-sv-comp` for streamlining this process:
* `goblint-all-fast.sh` – "quickly" (with 60s timeout) run Goblint on all ReachSafety (or more specifically SoftwareSystems) tasks without witness validation
* `goblint-data-race.sh` – run Goblint on all NoDataRace tasks
* `goblint.sh` (probably outdated) – run Goblint on all SoftwareSystems tasks with witness validation using CPAchecker and Ultimate Automizer

They have corresponding XML files for benchexec and table-generator.


---


# Goblint for SV-COMP (old)

## Run Goblint in SV-COMP mode
Command:
```
./goblint --enable ana.sv-comp --enable witness.uncil --enable ana.int.interval ./tests/sv-comp/basic/if_mod_true-unreach-call.c
```

There's a bunch of very simple files to test with in `./tests/sv-comp/` with the expected results in the filename (old SV-COMP task definition format).


## Witness-related Goblint flags
* `ana.sv-comp`

  Adds SV-COMP `__VERIFIER_*` functions, outputs verdict to stdout and outputs witness to `witness.graphml` in current (Goblint root) directory.

* `witness.path`

  Override witness output filename.

* `witness.uncil`

  Cil transforms `&&` and `||` into `if`s, which causes the witness to contain spurious conditional control edges where the original program didn't. I'm guessing this would mix up things for witness validators.

  This option does some hacks to try to undo that transformation when writing the witness.

* `witness.minimize`

  Minimizes the witness graph by skipping nodes and edges without any useful witness data to make witnesses easier to inspect.

  I don't guarantee this minimization to be correct (it may be too aggressive in some places?) nor useful (it may be not aggressive enough in other places).

* `ana.wp`

  Enables weakest precondition feasibility analysis for SV-COMP violations.


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



# Inspecting witnesses
## yEd

1. Open (Ctrl+o) `witness.graphml` from Goblint root directory.
2. Click menu "Edit" → "Properties Mapper".
    1. _First time:_  Click button "Imports additional configurations" and open `yed-sv-comp.cnfx` from this directory.
    2. Select "SV-COMP (Node)" and click "Apply".
    3. Select "SV-COMP (Edge)" and click "Ok".
3. Click menu "Layout" → "Hierarchial" (Alt+shift+h).
    1. _First time:_ Click tab "Labeling", select "Hierarchic" in "Edge Labeling".
    2. Click "Ok".

yEd manual for the Properties Mapper: https://yed.yworks.com/support/manual/properties_mapper.html.
