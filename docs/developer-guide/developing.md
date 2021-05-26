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
