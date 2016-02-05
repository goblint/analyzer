[![Stories in Ready](https://badge.waffle.io/goblint/analyzer.png?label=ready&title=Ready)](https://waffle.io/goblint/analyzer)
[![Build status](https://travis-ci.org/goblint/analyzer.png)](https://travis-ci.org/goblint/analyzer)
[![Circle CI](https://circleci.com/gh/goblint/analyzer.svg?style=svg)](https://circleci.com/gh/goblint/analyzer)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/goblint)
# goblint

## Setup
### Linux 
[Install opam](http://opam.ocaml.org/doc/Install.html) and then do

    git clone https://github.com/goblint/analyzer.git
    cd analyzer
    make setup

to install OCaml and the latest versions of the dependencies for the current user.
After that you can build goblint:

    make

If something goes wrong, take a look at [travis-ci.sh](scripts/travis-ci.sh) for an example setup.
You may now try running goblint: `./goblint tests/regression/04-mutex/01-simple_rc.c`.

### Mac OS X

The above instructions for Linux work just as well for Mac OS X. Goblint
successfully compiles, but it may fail to parse files. We need `cpp` in order
to preprocess source files and current versions of the clang frontend will not
work with goblint. You therefore have to do, e.g., `brew install gcc` (first do `xcode-select --install` if you don't want to build from source). You can check src/config.ml to see what command is used to call `cpp`.


### Windows/Cygwin
For a goblint binary package, follow [these instructions](http://goblint.in.tum.de/download.html).
For building from source, install Cygwin using [setup-cygwin-dev.bat](scripts/setup-cygwin-dev.bat) and then do

    wget "https://raw.githubusercontent.com/goblint/analyzer/master/scripts/setup-cygwin-dev.sh" && source setup-cygwin-dev.sh

### Virtual machine
A ready-to-use virtual machine can be set up using [Vagrant](http://www.vagrantup.com/):

    vagrant up
    vagrant ssh
    sudo su -
    cd analyzer

### Docker container
Running goblint in a [Docker](http://www.docker.com/) container is even faster since everything is already installed:

    docker pull voglerr/goblint
    docker run voglerr/goblint /analyzer/goblint --help

### Web frontend
Use `make npm` to setup the web frontend and start serving on <http://localhost:3000>.
See its [README](https://github.com/vogler/goblint-webapp) for details.

### g2html
Use `make jar` to build `g2html.jar`, which can be used like so

    ./goblint --html -o result path/to/file.c && firefox result/index.xml
