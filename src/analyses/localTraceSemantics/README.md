# Local Trace Analysis
Make use of solver WLLocTrac, which is based on a queue and does some priority calculation on the order of unknowns.\
Example command:
```bash
./goblint --enable warn.debug tests/regression/71-localTraces/33-nondeterministc-loop-exit.c  --set "ana.activated[+]" localTrac
es --set solver "WLLocTrac" --set warn_at "early" --set verify false --html >tmp.txt
```
Extra option for witness generation with specific filename:

--set witness.violation.filename "test-witness.graphml"

Enable casting overflow for signed types:

--set local-traces.int-overflow.cast true

Run the analysis with specification:
```bash
./goblint --enable warn.debug tests/witness/14-reach-error-loop.c  --set "ana.activated[+]" localTraces --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"  --set solver "WLLocTrac" --set warn_at "early" --set verify false --html > tmp.txt
```

Visualize with
```bash
python3 -m http.server --directory result 8080
```