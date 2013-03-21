# goblint

## Setup
Install [opam](https://github.com/OCamlPro/opam) [[Quick Install](http://opam.ocamlpro.com/doc/Quick_Install.html)], then do

    opam install ocamlfind camomile batteries cil xml-light

to install the latest versions of the dependencies.
After that you can build goblint:

    git clone https://github.com/goblint/analyzer.git
    cd analyzer
    make
  
If something goes wrong, switch to the versions listed in [INSTALL](INSTALL):

    opam switch 4.00.1
    opam install ocamlfind.1.3.3 camomile.0.8.3 batteries.2.0.0 cil.1.5.1 xml-light.2.2
