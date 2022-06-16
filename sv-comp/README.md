# Goblint for SV-COMP
All the SV-COMP configuration is in `conf/svcomp.json`.

## Run Goblint in SV-COMP mode
### ReachSafety
```
./goblint --conf conf/svcomp.json --set ana.specification ../sv-benchmarks/c/properties/unreach-call.prp ../sv-benchmarks/c/DIR/FILE.i
```

### NoDataRace
```
./goblint --conf conf/svcomp.json --set ana.specification ../sv-benchmarks/c/properties/no-data-race.prp ../sv-benchmarks/c/DIR/FILE.i
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
