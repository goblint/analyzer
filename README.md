# goblint &nbsp; [![Travis-CI](https://travis-ci.com/goblint/analyzer.png)](https://travis-ci.com/goblint/analyzer/branches) [![Circle CI](https://circleci.com/gh/goblint/analyzer.svg?style=svg)](https://circleci.com/gh/goblint/analyzer)

## Setup
[Install opam](http://opam.ocaml.org/doc/Install.html), and then do `opam install goblint` to install the latest release.

For development, use `make setup` to install OCaml and all dependencies, and `make` to build.

Run goblint: `./goblint tests/regression/04-mutex/01-simple_rc.c`.

If something goes wrong, take a look at [travis-ci.sh](scripts/travis-ci.sh) for an example Ubuntu/macOS setup.

### macOS
Goblint relies on GNU `cpp` to preprocess source files - the default clang `cpp` on macOS will not work.
You can install it with `brew install gcc` (first do `xcode-select --install` if you don't want to build from source). You can check src/config.ml to see what command is used to call `cpp`.

### Windows
We recommend using WSL/bash for Windows 10.
For older versions you can try to build using Cygwin.
For a goblint binary package, follow [these instructions](http://goblint.in.tum.de/download.html).
For building from source, install Cygwin using [setup-cygwin-dev.bat](scripts/setup-cygwin-dev.bat) and then do

    wget "https://raw.githubusercontent.com/goblint/analyzer/master/scripts/setup-cygwin-dev.sh" && source setup-cygwin-dev.sh

### Docker container
You can run Goblint in a [Docker container](https://hub.docker.com/r/voglerr/goblint/) using:

    docker run -it voglerr/goblint ./goblint --help

### Virtual machine
A virtual machine containing Goblint can be set up using [Vagrant](http://www.vagrantup.com/):

    vagrant up
    vagrant ssh
    sudo su -
    cd analyzer

### Web frontend
The analysis results are printed to stdout by default.
Adding `--html` saves the results as XML, which is then transformed to be viewable in a web browser.
Use `make jar` to build the needed Java program.

    ./goblint --html path/to/file.c && firefox result/index.xml

<!-- ### Web frontend -->
<!-- Use `make npm` to setup the web frontend and start serving on <http://localhost:3000>. -->
<!-- See its [README](https://github.com/vogler/goblint-webapp) for details. -->
