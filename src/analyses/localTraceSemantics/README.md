# Local Trace Analysis
Right now, solver "WL" should be used because the stack-based solver does not work yet properly.\
Example command:
```bash
./goblint --enable dbg.debug src/analyses/localTraceSemantics/examples/example1.c  --set "ana.activated[+]" localTraces --set solver "WL" --set warn_at "early" --set verify false --html >tmp.txt 
```