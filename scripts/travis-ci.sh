# This script runs the unit tests inside a Travis CI worker. See also .travis.yml
# Inspired by http://blog.mlin.net/2013/02/testing-ocaml-projects-on-travis-ci.html

# export OPAM_VERSION=1.0.0
export OCAML_VERSION=4.00.1
export OPAM_PACKAGES='ocamlfind camomile batteries cil xml-light'

# install non-ocaml dependencies (ocaml from here is too old)
sudo apt-get update -qq
sudo apt-get install -qq make #ruby code2html

# binary installer for opam and ocaml
wget http://www.ocamlpro.com/pub/opam_installer.sh
sh ./opam_installer.sh /usr/local/bin ${OCAML_VERSION} # Install the latest OPAM and OCaml
eval `opam config -env`

# install packages from opam
opam install -q -y ${OPAM_PACKAGES}

# compile
./make.sh
