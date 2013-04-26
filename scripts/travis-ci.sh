# This script runs the unit tests inside a Travis CI worker. See also .travis.yml
# Inspired by http://blog.mlin.net/2013/02/testing-ocaml-projects-on-travis-ci.html

export OPAM_VERSION=1.0.0
export OPAM_PACKAGES='ocamlfind camomile batteries cil xml-light'

# install ocaml from apt
sudo apt-get update -qq
sudo apt-get install -qq ocaml

# install opam
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz
pushd opam-${OPAM_VERSION}
./configure
make
sudo make install
popd
opam init --auto-setup
eval `opam config -env`

# install packages from opam
opam install -q -y ${OPAM_PACKAGES}

# compile
make
