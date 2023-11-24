# Releasing

## opam

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
    6. Install depexts: `opam depext --with-test goblint`.
    7. Install and test package: `opam install --with-test goblint`.
    8. Activate opam environment: `eval $(opam env)`.
    9. Check version: `goblint --version`.
    10. Check that analysis works: `goblint -v tests/regression/04-mutex/01-simple_rc.c`.
    11. Exit Docker container.

12. Temporarily enable Zenodo GitHub webhook.

    This is because we only want numbered version releases to automatically add a new version to our Zenodo artifact.
    Other tags (like SV-COMP or paper artifacts) have manually created Zenodo artifacts anyway and thus shouldn't add new versions to the main Zenodo artifact.

13. Create a GitHub release with the git tag: `DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib`.

    Explicitly specify `distrib` because we don't want to publish OCaml API docs.
    Environment variable workaround for the package having a Read the Docs `doc` URL (see <https://github.com/ocamllabs/dune-release/issues/154>).

14. Re-disable Zenodo GitHub webhook.

15. Create an opam package: `dune-release opam pkg`.
16. Submit the opam package to opam-repository: `dune-release opam submit`.


## SV-COMP

### Before all preruns

1. Make sure you are running the same Ubuntu version as will be used for SV-COMP.
2. Create conf file for SV-COMP year.
3. Make sure this repository is checked out into a directory called `goblint`, not the default `analyzer`.

    This is required such that the created archive would have everything in a single directory called `goblint`.

4. Update SV-COMP year in `sv-comp/archive.sh`.

    This includes: git tag name, git tag message and zipped conf file.

### For each prerun

1. Update opam pins:

    1. Make sure you have the same `goblint-cil` version pinned as `goblint.opam` specifies.
    2. Unpin `zarith.1.12-gob0`, because Gobview compatibility is not required.

2. Make sure you have nothing valuable that would be deleted by `make clean`.
3. Delete git tag from previous prerun: `git tag -d svcompXY`.
4. Create archive: `./sv-comp/archive.sh`.

    The resulting archive is `sv-comp/goblint.zip`.

5. Check unextracted archive in latest SV-COMP container image: <https://gitlab.com/sosy-lab/benchmarking/competition-scripts/#container-image>.

    Inside Docker:

    1. Check version: `./goblint --version`.
    2. Mount some sv-benchmarks and properties, e.g. as `/tool-test`, and run Goblint on them manually.

    This ensures that the environment and the archive have all the correct system libraries.

6. Create (or add new version) Zenodo artifact and upload the archive.

7. Open MR with Zenodo version DOI to the [fm-tools](https://gitlab.com/sosy-lab/benchmarking/fm-tools) repository.

<!-- 7. Check pushed archive via CoveriTeam-Remote: <https://gitlab.com/sosy-lab/software/coveriteam/-/blob/main/doc/competition-help.md>.

1. Clone coveriteam repository.
2. Locally modify `actors/goblint.yml` archive location to the raw URL of the pushed archive.
3. Run Goblint on some sv-benchmarks and properties via CoveriTeam.

This ensures that Goblint runs on SoSy-Lab servers. -->

### After all preruns

1. Push git tag from last prerun: `git push origin svcompXY`.
2. Create GitHub release from the git tag and attach latest submitted archive as a download.
3. Manually run `docker` workflow on `svcompXY` git tag and targeting `svcompXY` Docker tag.

    This is because the usual `docker` workflow only handles semver releases.

4. Release new semver version on opam. See above.
