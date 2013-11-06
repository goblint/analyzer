# goblint [![Build status](https://travis-ci.org/vogler/analyzer.png)](https://travis-ci.org/vogler/analyzer)

## Setup
Install [opam](https://github.com/OCamlPro/opam) [[Quick Install](http://opam.ocamlpro.com/doc/Quick_Install.html)], then do

    opam install ocamlfind camomile batteries cil xml-light

to install the latest versions of the dependencies for the current user.
After that you can build goblint:

    git clone https://github.com/vogler/analyzer.git
    cd analyzer
    make

If something goes wrong, take a look at [travis-ci.sh](scripts/travis-ci.sh) for an example setup or try the versions listed in [INSTALL](INSTALL).

Alternatively you can use your system's package manager to install the dependencies globally or use [install_script.sh](scripts/install_script.sh) to build everything from source without affecting any existing OCaml installation.

A ready-to-use virtual machine can be started using [Vagrant](http://www.vagrantup.com/):

    vagrant up
    vagrant ssh
    sudo su -
    cd analyzer

In order to setup the web frontend do

    git submodule update --init --recursive
    cd webapp

Then follow the instructions in its [README](https://github.com/vogler/goblint-webapp).
