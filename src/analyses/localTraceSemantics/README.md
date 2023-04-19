# Local Trace Analysis
Make use of solver WLLocTrac, which is based on a queue and does some priority calculation on the order of unknowns.\
Example command:
```bash
./goblint --enable dbg.debug src/analyses/localTraceSemantics/examples/example1.c  --set "ana.activated[+]" localTraces --set solver "WLLocTrac" --set warn_at "early" --set verify false --html >tmp.txt 
```
Visualize with
```bash
python3 -m http.server --directory result 8080
```