# Releasing

1. Update list of authors and contributors in `.zenodo.json`, `CITATION.cff` and `dune-project`.
2. Update `CHANGELOG.md`:

    1. Add a desired version number (`vX.Y.Z`) header at the top.
    2. Add a list of biggest changes compared to the previous version.

3. Install dune-release: `opam install dune-release`.
4. Remove all opam pins because _opam-repository doesn't allow them_.

    * If the pinned changes have been released and published in opam, remove the pin (and add a version lower bound).
    * If the pinned changes are not strictly necessary for building (but just optimization or stability), then temporarily remove the pin.

5. Regenerate `goblint.opam`: `dune build`.
6. Regenerate `goblint.opam.locked`: `opam pin add goblint.dev . --no-action` and `opam lock .`.

    Pinning the package is necessary for locking, otherwise lockfile will be generated for previously published version.
    Manually remove not installed `depopts` from `conflicts`.

7. Check with `dune-release check`.

    All changes must be committed because the working tree is not checked.

8. Check that "unlocked" workflow passes on GitHub Actions.

    It can be run manually on the release branch for checking.

9. Tag the release: `dune-release tag`.
10. Create the distribution archive: `dune-release distrib`.

11. Check created _distribution archive_ (in `_build`) in a clean environment:

    1. Pull Docker image: `docker pull ocaml/opam:ubuntu-22.04-ocaml-4.14` (or newer).
    2. Extract distribution archive.
    3. Run Docker container in extracted directory: `docker run -it --rm -v $(pwd):/goblint ocaml/opam:ubuntu-22.04-ocaml-4.14` (or newer).
    4. Navigate to distribution archive inside Docker container: `cd /goblint`.
    5. Pin package from distribution archive: `opam pin add --no-action .`.
    6. Install depexts: `opam depext goblint`.
    7. Install and test package: `opam install --with-test goblint`.
    8. Activate opam environment: `eval $(opam env)`.
    9. Check version: `goblint --version`.
    10. Check that analysis works: `goblint -v tests/regression/04-mutex/01-simple_rc.c`.
    11. Exit Docker container.

12. Create a GitHub release with the git tag: `DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib`.

    Explicitly specify `distrib` because we don't want to publish OCaml API docs.
    Environment variable workaround for the package having a Read the Docs `doc` URL (see <https://github.com/ocamllabs/dune-release/issues/154>).

13. Create an opam package: `dune-release opam pkg`.
14. Submit the opam package to opam-repository: `dune-release opam submit`.
