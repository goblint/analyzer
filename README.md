# Goblint
[![GitHub release status](https://img.shields.io/github/v/release/goblint/analyzer)](https://github.com/goblint/analyzer/releases)
[![opam package status](https://badgen.net/opam/v/goblint)](https://opam.ocaml.org/packages/goblint)
[![Zenodo DOI](https://zenodo.org/badge/2066905.svg)](https://zenodo.org/badge/latestdoi/2066905)

[![locked workflow status](https://github.com/goblint/analyzer/actions/workflows/locked.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/locked.yml)
[![unlocked workflow status](https://github.com/goblint/analyzer/actions/workflows/unlocked.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/unlocked.yml)
[![Coverage Status](https://coveralls.io/repos/github/goblint/analyzer/badge.svg?branch=master)](https://coveralls.io/github/goblint/analyzer?branch=master)
[![docker workflow status](https://github.com/goblint/analyzer/actions/workflows/docker.yml/badge.svg)](https://github.com/goblint/analyzer/actions/workflows/docker.yml)
[![Documentation Status](https://readthedocs.org/projects/goblint/badge/?version=latest)](https://goblint.readthedocs.io/en/latest/?badge=latest)
[![project chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://goblint.zulipchat.com)

Documentation can be browsed on [Read the Docs](https://goblint.readthedocs.io/en/latest/) or [GitHub](./docs/).

## Installing
Both for using an up-to-date version of Goblint or developing it, the best way is to install from source by cloning this repository.
For benchmarking Goblint, please follow the [Benchmarking guide on Read the Docs](https://goblint.readthedocs.io/en/latest/user-guide/benchmarking/).

### Linux
1. Install [opam](https://opam.ocaml.org/doc/Install.html).
2. Make sure the following are installed: `git`, `patch`, `m4`, `autoconf`, `libgmp-dev`, `libmpfr-dev` and `pkg-config`.
3. Run `make setup` to install OCaml and dependencies via opam.
4. Run `make` to build Goblint itself.
5. Run `make install` to install Goblint into the opam switch for usage via switch's `PATH`.
6. _Optional:_ See [`scripts/bash-completion.sh`](./scripts/bash-completion.sh) for setting up bash completion for Goblint arguments.

### MacOS
1. Install GCC with `brew install gcc grep` (first run `xcode-select --install` if you don't want to build it from source). Goblint requires GCC while macOS's default `cpp` is Clang, which will not work.
2. ONLY for M1 (ARM64) processor: homebrew changed its install location from `/usr/local/` to `/opt/homebrew/`. For packages to find their dependecies execute `sudo ln -s /opt/homebrew/{include,lib} /usr/local/`.
3. Continue using Linux instructions (the formulae in brew for `patch`, `libgmp-dev`, `libmpfr-dev` are `gpatch`, `gmp`, `mpfr`, respectively).

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

## Acknowledgements

Work on Goblint was supported in part by Deutsche Forschungsgemeinschaft (DFG) (47140942/1480 [PUMA](https://gepris.dfg.de/gepris/projekt/4714094), 378803395/2428 [ConVeY](http://convey.in.tum.de)), ARTEMIS Joint Undertaking (269335 [MBAT](http://www.mbat-artemis.eu/home/)), ITEA3 project 14014 [ASSUME](http://assume-project.eu/), the Shota Rustaveli National Science Foundation of Georgia [FR-21-7973](https://viam.science.tsu.ge/new/index.php?lang=eng&page=projects&subpage=111), the Estonian Research Council ([IUT2-1](https://www.etis.ee/Portal/Projects/Display/561b7b1d-d1dd-43a2-90e5-0661de823823?lang=ENG), [PSG61](https://www.etis.ee/Portal/Projects/Display/743243bb-15c2-47b3-9c10-e7d86a9a276d?lang=ENG)), and the Estonian Centre of Excellence in IT (EXCITE), funded by the European Regional Development Fund.

We also thank [Zulip](https://zulip.com) for providing free Zulip Cloud Standard hosting for the Goblint project. Zulip is an open-source modern team chat app designed to keep both live and asynchronous conversations organized.