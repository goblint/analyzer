# Goblint
[![locked workflow status](https://github.com/goblint/analyzer/actions/workflows/locked.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/locked.yml)
[![unlocked workflow status](https://github.com/goblint/analyzer/actions/workflows/unlocked.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/unlocked.yml)
[![docker workflow status](https://github.com/goblint/analyzer/actions/workflows/docker.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/docker.yml)
[![Documentation Status](https://readthedocs.org/projects/goblint/badge/?version=latest)](https://goblint.readthedocs.io/en/latest/?badge=latest)
[![Zenodo DOI](https://zenodo.org/badge/2066905.svg)](https://zenodo.org/badge/latestdoi/2066905)

Documentation can be browsed on [Read the Docs](https://goblint.readthedocs.io/en/latest/) or [GitHub](./docs/).

## Installing
Both for using an up-to-date version of Goblint or developing it, the best way is to install from source by cloning this repository.

### Linux
1. Install [opam](https://opam.ocaml.org/doc/Install.html).
2. Make sure the following are installed: `git patch m4 autoconf libgmp-dev libmpfr-dev pkg-config`.
3. Run `make setup` to install OCaml and dependencies via opam.
4. Run `make` to build Goblint itself.
5. Run `make install` to install Goblint into the opam switch for usage via switch's `PATH`.

### MacOS
1. Install GCC with `brew install gcc` (first run `xcode-select --install` if you don't want to build it from source). Goblint requires GCC while macOS's default `cpp` is Clang, which will not work.
2. ONLY for M1 (ARM64) processor: homebrew changed its install location from `/usr/local/` to `/opt/homebrew/`. For packages to find their dependecies execute `sudo ln -s /opt/homebrew/{include,lib} /usr/local/`.
3. Continue using Linux instructions (the formulae in brew for `patch libgmp-dev libmpfr-dev` are `gpatch gmp mpfr`, respectively).

### Windows
1. Install [WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10). Goblint is not compatible with WSL1.
2. Continue using Linux instructions in WSL.

### Other
* **[opam](https://opam.ocaml.org/packages/goblint/)**. Install [opam](https://opam.ocaml.org/doc/Install.html) and run `opam install goblint`.
* **[devcontainer](./.devcontainer/).** Select "Reopen in Container" in VS Code and continue with `make` using Linux instructions in devcontainer.
* **[Docker (GitHub Container Registry)](https://github.com/goblint/analyzer/pkgs/container/analyzer)**. Run `docker pull ghcr.io/goblint/analyzer:latest` (or `:nightly`).
* **Docker (repository).** Clone and run `docker build -t goblint .`.
* **Vagrant.** Clone and run `vagrant up && vagrant ssh`.


## Running
To confirm that building worked, you can try running Goblint as follows:
```
./goblint tests/regression/04-mutex/01-simple_rc.c
```
To confirm that installation into the opam switch worked, you can try running Goblint as follows:
```
goblint tests/regression/04-mutex/01-simple_rc.c
```
To confirm that the Docker container worked, you can try running Goblint as follows:
```
docker run -it --rm -v $(pwd):/data goblint /data/tests/regression/04-mutex/01-simple_rc.c
```
If pulled from GitHub Container Registry, use the container name `ghcr.io/goblint/analyzer:latest` (or `:nightly`) instead.

For further information, see [documentation](https://goblint.readthedocs.io/en/latest/user-guide/running/).
