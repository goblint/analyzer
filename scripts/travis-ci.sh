# This script is for either running in a Travis CI worker or the VM setup by vagrant. See .travis.yml and Vagrantfile
# Inspired by https://github.com/lunaryorn/flycheck

# switch to root
#sudo su -

export OPAM_VERSION=1.0.0
# export OCAML_VERSION=4.00.1
export OPAM_PACKAGES='ocamlfind.1.3.3 camomile.0.8.3 batteries.2.0.0 xml-light.2.2'

ppa () {
    for ppa in "$@"; do
        sudo apt-add-repository -y "$ppa"
    done
}
apt_update () {
    sudo apt-get update -qq
}
apt () {
    sudo apt-get install -yy --fix-missing "$@"
}

# Silence debconf
export DEBIAN_FRONTEND='noninteractive'

# Update repositories to prevent errors caused by missing packages
apt_update

# Needed for ppa
apt python-software-properties

# Bring in the necessary PPAs and 3rd party repositories
# ocaml in standard Ubuntu repo is too old and compiling with opam takes too long
ppa ppa:mike-mcclurg/ocaml # version 4.00.0
apt_update
apt ocaml
apt make m4  # needed for compiling ocamlfind
apt patch    # needed for compiling xml-light
apt autoconf # needed for compiling cil
apt git      # needed for cloning goblint source

# binary installer for opam (append -a to version for --auto-setup)
if hash opam 2>/dev/null; then
    echo "OPAM already installed"
else
    wget http://www.ocamlpro.com/pub/opam_installer.sh
    sudo sh ./opam_installer.sh /usr/local/bin "system -a" # Install the latest OPAM using the system compiler
fi

# install opam from source and then switch to OCAML_VERSION
# curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz
# pushd opam-${OPAM_VERSION}
# ./configure
# make
# sudo make install
# popd
# rm -rf opam-${OPAM_VERSION} # otherwise ocamlbuild complains about hygiene violations
# opam init --auto-setup --comp ${OCAML_VERSION}
eval `opam config -env`
#sudo sh -c 'eval `opam config -env`' # needed for cil: sudo make install needs ocamlfind on path

# install packages from opam
sudo chown -hR `eval whoami` ~/.opam # fix permissions (owned by root because of sudo ./opam_installer.sh)
opam install -q -y ${OPAM_PACKAGES}

# clone current version of cil (1.5.1 from opam is apparently too old)
if hash cilly 2>/dev/null; then
    echo "CIL is already installed:"
    cilly --version
else
   git clone git://cil.git.sourceforge.net/gitroot/cil/cil
   pushd cil
   ./configure && make && sudo -E make install
   popd
   rm -rf cil
fi

# clone goblint (if not running in travis-ci)
if test -e "make.sh"; then
    echo "already in repository"
else
    git clone https://github.com/vogler/analyzer.git
    pushd analyzer
fi

# compile
./make.sh
