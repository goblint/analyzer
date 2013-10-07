# This script is for either running in a Travis CI worker or the VM setup by vagrant. See .travis.yml and Vagrantfile
# Inspired by https://github.com/lunaryorn/flycheck

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
apt make m4  # needed for compiling ocamlfind
apt patch    # needed for compiling xml-light
apt autoconf # needed for compiling cil
apt git      # needed for cloning goblint source


# http://anil.recoil.org/2013/09/30/travis-and-ocaml.html
OPAM_DEPENDS="ocamlfind camomile batteries xml-light cil"
   
case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac
   
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`


# clone goblint (if not running in travis-ci)
if test -e "make.sh"; then
    echo "already in repository"
else
    git clone https://github.com/vogler/analyzer.git
    pushd analyzer
fi

# compile
./make.sh
