#/bin/bash

#Ubuntu: sudo apt-get install git m4 autoconf ruby code2html
#CYGWIN: setup.exe -P git,wget,unzip,make,m4,gcc,gcc4-core,libmpfr4,autoconf,flexdll,libncurses-devel,ruby,code2html

BINDIR=`pwd`/usr_tmp
export PATH=$BINDIR/bin:$PATH

if [ -d scripts ]; then 
  echo "Existing Goblint installation detected!"
  echo "This is a stand-alone install script. Please run in an empty directory."
  exit 1
fi

echo "Fetching..."
if [ -d analyzer ]; then
  cd analyzer && git pull && cd .. || exit 1
else
  git clone git://github.com/goblint/analyzer.git || exit 1
fi 
if [ -d bench ]; then
  cd bench && git pull && cd .. || exit 1
else
  git clone git://github.com/goblint/bench.git || exit 1
fi
if [ -d cil ]; then
  cd cil && git pull && cd .. || exit 1
else
  git clone git://cil.git.sourceforge.net/gitroot/cil/cil || exit 1
fi

mkdir -p archives
cd archives
wget -c http://caml.inria.fr/pub/distrib/ocaml-4.00/ocaml-4.00.1.tar.gz || exit 1
wget -c http://download.camlcity.org/download/findlib-1.3.3.tar.gz || exit 1
wget -c http://prdownloads.sourceforge.net/camomile/camomile-0.8.3.tar.bz2 || exit 1
wget -c http://forge.ocamlcore.org/frs/download.php/1096/batteries-2.0.tar.gz || exit 1
wget -c http://tech.motion-twin.com/zip/xml-light-2.2.zip || exit 1
cd ..
echo "================================================================================"

echo "Extracting..."
mkdir -p dependencies
cd dependencies
tar xzfk ../archives/ocaml-4.00.1.tar.gz || exit 1
tar xzfk ../archives/findlib-1.3.3.tar.gz || exit 1
tar xjfk ../archives/camomile-0.8.3.tar.bz2 || exit 1
tar xzfk ../archives/batteries-2.0.tar.gz || exit 1
unzip -qn ../archives/xml-light-2.2.zip || exit 1
echo "================================================================================"

echo "Installing Ocaml 4.00.1"
mkdir $BINDIR
cd ocaml-4.00.1
./configure -prefix $BINDIR && make world.opt && make install || exit 1
cd ..
echo "================================================================================"

echo "Installing findlib 1.3.3"
cd findlib-1.3.3
./configure && make all && make opt && make install || exit 1
cd ..
echo "================================================================================"

echo "Installing camomile 0.8.3"
cd camomile-0.8.3
./configure -prefix $BINDIR && make && make install || exit 1
cd ..
echo "================================================================================"

echo "Installing batteries 2.0"
cd batteries-2.0
make && make install || exit 1
cd ..
echo "================================================================================"

echo "Installing xml-light 2.2"
cd xml-light
make 
make && make opt || exit 1
echo 'version="2.2"' > META
echo 'archive(byte)="xml-light.cma"' >> META
echo 'archive(native)="xml-light.cmxa"' >> META
ocamlfind install xml-light META *.cmx *.cmi *.mli *.a *.o *.cmxa *.cma || exit 1
cd ..
cd ..
echo "================================================================================"

echo "Installing Cil"
cd cil
./configure -prefix $BINDIR && make && make install || exit 1
cd ..
echo "================================================================================"

echo "Building Goblint!!!"
cd analyzer
make || exit 1
./scripts/update_suite.rb
echo "================================================================================"
echo "If you need to recompile, add usr_tmp to PATH:"
echo "   echo 'export PATH=$BINDIR/bin:\$PATH' >> ~/.bashrc"
echo "================================================================================"
