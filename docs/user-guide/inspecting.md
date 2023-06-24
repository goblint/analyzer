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

1. Run `dune build gobview` in the analyzer directory to build the web UI
2. The executable for the http-server can then be found in the directory `./_build/default/gobview/goblint-http-server`. It takes the analyzer directory and additional Goblint configurations such as the files to be analyzed as parameters. Run it e.g. with the following command:\
`./_build/default/gobview/goblint-http-server/goblint_http.exe -with-goblint ../analyzer/goblint -goblint --set files[+] "../analyzer/tests/regression/00-sanity/01-assert.c"`

4. Visit <http://localhost:8000>
