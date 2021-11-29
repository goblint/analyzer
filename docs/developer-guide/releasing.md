# Releasing

1. Install dune-release: `opam install dune-release`.
2. Remove all opam pins because _opam-repository doesn't allow them_.

    * If the pinned changes have been released and published in opam, remove the pin (and add a version lower bound).
    * If the pinned changes are not strictly necessary for building (but just optimization or stability), then temporarily remove the pin.

3. Regenerate `goblint.opam`: `dune build`.
4. Regenerate `goblint.opam.locked`: `opam pin add goblint.dev . --no-action` and `opam lock .`.

    Pinning the package is necessary for locking, otherwise lockfile will be generated for previously published version.
    Manually remove not installed `depopts` from `conflicts`.

5. Check with `dune-release check`.

    All changes must be committed because the working tree is not checked.

6. Check that "unlocked" workflow passes on GitHub Actions.

    It can be run manually on the release branch for checking.

7. Update `CHANGELOG.md`:

    1. Add a desired version number (`vX.Y.Z`) header at the top.
    2. Add a list of biggest changes compared to the previous version.

8. Update list of authors in `CITATION.cff` and `dune-project`.
9. Tag the release: `dune-release tag`.
10. Create the distribution archive: `dune-release distrib`.
11. Create a GitHub release with the git tag: `DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib`.

    Explicitly specify `distrib` because we don't want to publish OCaml API docs.
    Environment variable workaround for the package having a Read the Docs `doc` URL (see <https://github.com/ocamllabs/dune-release/issues/154>).

12. Create an opam package: `dune-release opam pkg`.
13. Submit the opam package to opam-repository: `dune-release opam submit`.
