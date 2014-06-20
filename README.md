[![Stories in Ready](https://badge.waffle.io/goblint/analyzer.png?label=ready&title=Ready)](https://waffle.io/goblint/analyzer)
[![Build status](https://travis-ci.org/goblint/analyzer.png)](https://travis-ci.org/goblint/analyzer)
[gitter](https://gitter.im/goblint)
# goblint

## Setup
### Linux, OS X
Install [opam](https://github.com/OCamlPro/opam) [[Quick Install](http://opam.ocamlpro.com/doc/Quick_Install.html)], then do

    opam install ocamlfind camomile batteries cil xml-light

to install the latest versions of the dependencies for the current user.
After that you can build goblint:

    git clone https://github.com/goblint/analyzer.git
    cd analyzer
    make

If something goes wrong, take a look at [travis-ci.sh](scripts/travis-ci.sh) for an example setup or try the versions listed in [INSTALL](INSTALL).

Alternatively you can use your system's package manager to install the dependencies globally or use [install_script.sh](scripts/install_script.sh) to build everything from source without affecting any existing OCaml installation.

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
