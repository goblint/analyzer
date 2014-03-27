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

    wget "https://raw.githubusercontent.com/goblint/analyzer/master/scripts/setup-cygwin-dev.sh" && source ./setup-cygwin-dev.sh

### Virtual machine
A ready-to-use virtual machine can be started using [Vagrant](http://www.vagrantup.com/):

    vagrant up
    vagrant ssh
    sudo su -
    cd analyzer

### Web frontend
In order to setup the web frontend do

    git submodule update --init --recursive
    cd webapp

Then follow the instructions in its [README](https://github.com/vogler/goblint-webapp).
