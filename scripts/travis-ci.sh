# This script runs the unit tests inside a Travis CI worker. See also .travis.yml
# Inspired by http://blog.mlin.net/2013/02/testing-ocaml-projects-on-travis-ci.html

export OPAM_VERSION=1.0.0
export OCAML_VERSION=4.00.1
export OPAM_PACKAGES='ocamlfind.1.3.3 camomile.0.8.3 batteries.2.0.0 cil.1.5.1 xml-light.2.2'

# install dependencies (but use experimental because the default repo's ocaml version is too old)
# echo "deb http://ftp.de.debian.org/debian experimental main" >> /etc/apt/sources.list
sudo apt-get update -qq
sudo apt-get install -qq ocaml curl make m4 #ruby code2html

# binary installer for opam (doesn't work with travis because of promt from `opam init`)
#wget http://www.ocamlpro.com/pub/opam_installer.sh
#sh ./opam_installer.sh /usr/local/bin system # Install the latest OPAM using the system compiler (if any)

# install opam from source
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz
pushd opam-${OPAM_VERSION}
./configure
make
sudo make install
popd
rm -rf opam-${OPAM_VERSION} # otherwise ocamlbuild complains about hygiene violations
opam init --auto-setup
eval `opam config -env`

# install packages from opam
opam install -q -y ${OPAM_PACKAGES}

# compile
./make.sh
