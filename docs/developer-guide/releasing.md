# Releasing

1. Install dune-release: `opam install dune-release`.
2. Remove all opam pins because _opam-repository doesn't allow them_.

    * If the pinned changes have been released and published in opam, remove the pin (and add a version lower bound).
    * If the pinned changes are not strictly necessary for building (but just optimization or stability), then temporarily remove the pin.

3. Regenerate `goblint.opam`: `dune build`.
4. Regenerate `goblint.opam.locked`: `opam lock .`.
5. Update `CHANGELOG.md`:

    1. Add a desired version number (`vX.Y.Z`) header at the top.
    2. Add a list of biggest changes compared to the previous version.

6. Tag the release: `dune-release tag`.
7. Do the release: `dune-release`.

    This will automatically do all of the following:

    1. Create the distribution archive (`dune-release distrib`).
    2. Create a GitHub release with the git tag (`dune-release publish`).
    3. Create an opam package (`dune-release opam pkg`).
    4. Submit the opam package to opam-repository (`dune-release opam submit`).
