# Inspecting results

## g2html
1. First time run: `make jar`.
2. Run Goblint with additional `--html` argument.
3. Run `python3 -m http.server --directory result 8080`
   or  `npx http-server -c-1 result`.
4. Inspect results at <http://localhost:8080/index.xml>.

Modern browsers' security settings forbid some file access which is necessary for g2html to work, hence the need for serving the results via Python's `http.server` (or similar).

## GobView

For the initial setup:

1. Install Node.js (preferably ≥ 12.0.0) and npm (≥ 5.2.0)
2. Run `make setup_gobview` in the analyzer directory

To build GobView (also for development):

1. Run `make view` in the analyzer directory to build the web UI
2. The executable `goblint-http.exe` takes the analyzer directory and additional Goblint configurations such as the files to be analyzed as parameters. Run it e.g. with the following command:\
`./goblint-http.exe tests/regression/00-sanity/01-assert.c`
3. Visit <http://localhost:8080>

## Witnesses

### GraphML

#### yEd

1. Open (Ctrl+o) `witness.graphml` from Goblint root directory.
2. Click menu "Edit" → "Properties Mapper".
    1. _First time:_  Click button "Imports additional configurations" and open `scripts/sv-comp/yed-sv-comp.cnfx`.
    2. Select "SV-COMP (Node)" and click "Apply".
    3. Select "SV-COMP (Edge)" and click "Ok".
3. Click menu "Layout" → "Hierarchial" (Alt+shift+h).
    1. _First time:_ Click tab "Labeling", select "Hierarchic" in "Edge Labeling".
    2. Click "Ok".

yEd manual for the Properties Mapper: <https://yed.yworks.com/support/manual/properties_mapper.html>.
