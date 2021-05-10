# Inspecting results

## g2html
1. First time run: `make jar`.
2. Run Goblint with additional `--html` argument.
3. Run in `./result` directory: `python3 -m http.server` or `npx http-server`.
4. Inspect results at <http://localhost:8080>.

Modern browsers' security settings forbid some file access which is necessary for g2html to work, hence the need for serving the results via Python's `http.server` (or similar).

If you are on macOS, instead of steps 3 and 4, you can also run `open result/index.xml -a Safari`.
