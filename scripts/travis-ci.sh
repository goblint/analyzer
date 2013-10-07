# This script is for either running in a Travis CI worker or the VM setup by vagrant. See .travis.yml and Vagrantfile
# Inspired by https://github.com/lunaryorn/flycheck

# setup base system and clone goblint if not running in travis-ci
if test -e "make.sh"; then # travis-ci
    echo "already in repository"
    # USER=`whoami`
else # vagrant
    apt () {
        sudo apt-get install -yy --fix-missing "$@"
    }
    # update repositories to prevent errors caused by missing packages
    sudo apt-get update -qq
    apt python-software-properties # needed for ppa
    apt make m4  # needed for compiling ocamlfind
    apt patch    # needed for compiling xml-light
    apt autoconf # needed for compiling cil
    apt git      # needed for cloning goblint source

    # USER=vagrant # provisioning is done as root, but ssh login is 'vagrant'
    cd /root # just do everything as root and later use 'sudo su -' for ssh
    if test ! -e "analyzer"; then # ignore if source already exists
        git clone https://github.com/vogler/analyzer.git
        # chown -hR $USER:$USER analyzer # make ssh user the owner
    fi
    pushd analyzer
fi


# install ocaml and friends, see http://anil.recoil.org/2013/09/30/travis-and-ocaml.html
OPAM_DEPENDS="ocamlfind camomile batteries xml-light cil"

# use default versions if none are set in environment
case "${OCAML_VERSION:=4.01.0},${OPAM_VERSION:=1.0.0}" in
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


# compile
./make.sh
