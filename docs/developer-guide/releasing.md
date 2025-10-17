# Releasing

## opam

1. Choose version name (collaboratively).
2. Update list of authors and contributors in `.zenodo.json`, `CITATION.cff` and `dune-project`.
3. Update `CHANGELOG.md`:

    1. Add a desired version number (`vX.Y.Z`) and name header at the top.
    2. Add a list of biggest changes compared to the previous version.

4. Remove all opam pins because _opam-repository doesn't allow them_.

    * If the pinned changes have been released and published in opam, remove the pin (and add a version lower bound).
    * If the pinned changes are not strictly necessary for building (but just optimization or stability), then temporarily remove the pin.

5. Check opam file for previous release on opam-repository for changes.
6. Regenerate `goblint.opam`: `dune build`.
7. Regenerate `goblint.opam.locked`: `opam pin add goblint.dev . --no-action` and `opam lock .`.

    Pinning the package is necessary for locking, otherwise lockfile will be generated for previously published version.
    Manually remove not installed `depopts` from `conflicts`.

8. Install dune-release: `opam install dune-release`.

9. Check with `dune-release check`.

    All changes must be committed because the working tree is not checked.

    The warning `[FAIL] opam field doc cannot be parsed by dune-release` is fine and can be ignored (see <https://github.com/ocamllabs/dune-release/issues/154>).

10. Check that "unlocked" workflow passes on GitHub Actions.

    It can be run manually on the release branch for checking.

11. Tag the release: `dune-release tag`.
12. Create the distribution archive: `dune-release distrib`.

13. Check created _distribution archive_ (in `_build`) in a clean environment:

    1. Pull Docker image: `docker pull ocaml/opam:ubuntu-22.04-ocaml-4.14` (or newer).
    2. Extract distribution archive.
    3. Run Docker container in extracted directory: `docker run -it --rm -v $(pwd):/goblint ocaml/opam:ubuntu-22.04-ocaml-4.14` (or newer).
    4. Update opam-repository from git: `opam-2.1 repository add git git+https://github.com/ocaml/opam-repository.git && opam-2.1 update`.
    5. Navigate to distribution archive inside Docker container: `cd /goblint`.
    6. Install and test package from distribution archive: `opam-2.1 install --with-test .`.
    7. Activate opam environment: `eval $(opam env)`.
    8. Check version: `goblint --version`.
    9. Check that analysis works: `goblint -v tests/regression/04-mutex/01-simple_rc.c`.
    10. Exit Docker container.

14. Temporarily enable Zenodo GitHub webhook.

    This is because we only want numbered version releases to automatically add a new version to our Zenodo artifact.
    Other tags (like SV-COMP or paper artifacts) have manually created Zenodo artifacts anyway and thus shouldn't add new versions to the main Zenodo artifact.

15. Create a GitHub release with the git tag: `DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib`.

    Explicitly specify `distrib` because we don't want to publish OCaml API docs.
    Environment variable workaround for the package having a Read the Docs `doc` URL (see <https://github.com/ocamllabs/dune-release/issues/154>).

16. Re-disable Zenodo GitHub webhook.

17. Edit GitHub release to add version name.
18. Create an opam package: `dune-release opam pkg`.
19. Submit the opam package to opam-repository: `dune-release opam submit`.
20. Revert temporary removal of opam pins.


## SV-COMP

### Before all preruns

1. Make sure you are running the same Ubuntu version as will be used for SV-COMP.
2. Create conf file for SV-COMP year.
3. Make sure this repository is checked out into a directory called `goblint`, not the default `analyzer`.

    This is required such that the created archive would have everything in a single directory called `goblint`.

4. Update SV-COMP year in `Makefile`, `scripts/sv-comp/archive.sh`, `scripts/sv-comp/smoketest.sh` and `scripts/sv-comp/Dockerfile`.

    This includes: git tag name, git tag message and zipped conf file.

5. Open MR with conf file name to the [bench-defs](https://gitlab.com/sosy-lab/sv-comp/bench-defs) repository.

### For each prerun

1. Update opam pins:

    1. Make sure you have the same `goblint-cil` version pinned as `goblint.opam` specifies.
    2. Unpin `zarith.1.12-gob0`, because Gobview compatibility is not required.

2. Make sure you have nothing valuable that would be deleted by `make clean`.
3. Delete git tag from previous prerun: `git tag -d svcompXY`.
4. Create archive: `./scripts/sv-comp/archive.sh`.

    The resulting archive is `scripts/sv-comp/goblint.zip`.

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
