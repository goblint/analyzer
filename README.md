# Goblint
[![locked workflow status](https://github.com/goblint/analyzer/actions/workflows/locked.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/locked.yml)
[![unlocked workflow status](https://github.com/goblint/analyzer/actions/workflows/unlocked.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/unlocked.yml)
[![Documentation Status](https://readthedocs.org/projects/goblint/badge/?version=latest)](https://goblint.readthedocs.io/en/latest/?badge=latest)
[![Docker Build Status](https://img.shields.io/docker/cloud/build/voglerr/goblint)](https://hub.docker.com/r/voglerr/goblint)

Documentation can be browsed on [Read the Docs](https://goblint.readthedocs.io/en/latest/) or [GitHub](./docs/).

## Installing
Both for using an up-to-date version of Goblint or developing it, the best way is to install from source by cloning this repository.

_The [goblint package on opam](https://opam.ocaml.org/packages/goblint/) is very outdated and should currently not be used._

### Linux
1. Install [opam](https://opam.ocaml.org/doc/Install.html).
2. Run `make setup` to install OCaml and dependencies via opam.
3. Run `make` to build Goblint itself.

### MacOS
1. Install GCC with `brew install gcc` (first run `xcode-select --install` if you don't want to build it from source). Goblint requires GCC while macOS's default `cpp` is Clang, which will not work.
2. Continue using Linux instructions.

### Windows
1. Install [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10).
2. Continue using Linux instructions in WSL.

### Other
* **Docker.** Clone and run `make docker`.
* **Vagrant.** Clone and run `vagrant up && vagrant ssh`.
* **[Docker Hub](https://hub.docker.com/r/voglerr/goblint)** (outdated). Run `docker run -it voglerr/goblint bash`.
* **[opam](https://opam.ocaml.org/packages/goblint/)** (very outdated). Run `opam install goblint`.


## Running
To confirm that the installation worked, you can try running Goblint as follows:
```
./goblint tests/regression/04-mutex/01-simple_rc.c
```

For further information, see [documentation](https://goblint.readthedocs.io/en/latest/user-guide/running/).
