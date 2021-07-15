# Developing

Running `make dev` does the following:

1. Installs some additional opam packages useful for developing.
2. Installs a Git pre-commit hook for ocp-indent (see below).

## Git
### Pre-commit hook
The pre-commit hook installed by `make dev` runs ocp-indent on the staged changes to check that they're correctly indented.
If it deems something incorrectly indented, it will output a diff showing where and how, and it will prevent the commit.
In VSCode you have to click "Show Command Output" in the error popup to see the diff.

If necessary (e.g. you think ocp-indent is wrong or wants you to fix unrelated code), the hook can be bypassed using `git commit -n`.
In such case also let us know about the inconvenience on the following GitHub issue, so we can work towards improving the process: <https://github.com/goblint/analyzer/issues/236>.


## Opam
### CIL
The dependency on Goblint's CIL fork (`goblint-cil`) is _pinned_ in Opam to its repository instead of using the version published to Opam.
This speeds up the development process as every change in CIL doesn't require publishing a new version to Opam.

When CIL is changed and the pin is updated, the upgrade must also be performed in the local Opam switch by running the following commands:

1. Update pinned CIL version: `opam install --deps-only .` (note the `.` argument). Otherwise upgrading won't actually do anything.
2. Actually upgrade CIL: `opam upgrade goblint-cil`.
