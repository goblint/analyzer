# Inspecting results

## g2html
1. First time run: `make jar`.
2. Run Goblint with additional `--html` argument.
3. Run `python3 -m http.server --directory result 8080`
   or  `npx http-server -c-1 result`.
4. Inspect results at <http://localhost:8080/index.xml>.

Modern browsers' security settings forbid some file access which is necessary for g2html to work, hence the need for serving the results via Python's `http.server` (or similar).

## Gobview

For the initial setup:

1. Install Node.js (preferably ≥ 12.0.0) and npm (≥ 5.2.0)
2. Run `make setup_gobview` in the analyzer directory

To build Gobview (also for development):

1. Run `dune build gobview` in the analyzer directory to build the web UI
2. Run Goblint with these flags: `--enable gobview --set save_run DIR` (`DIR` is the name of the result directory that Goblint will create and populate, if not specified it is `run`)
3. `cd` into `DIR` and run `python3 -m http.server`
4. Visit <http://localhost:8000>
