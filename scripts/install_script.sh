#/bin/bash

sudo apt-get install git m4 autoconf

echo "Fetching..."
git clone git://github.com/goblint/analyzer.git
git clone git://cil.git.sourceforge.net/gitroot/cil/cil
mkdir archives && cd archives
wget http://caml.inria.fr/pub/distrib/ocaml-4.00/ocaml-4.00.1.tar.gz
wget http://download.camlcity.org/download/findlib-1.3.3.tar.gz
wget http://prdownloads.sourceforge.net/camomile/camomile-0.8.3.tar.bz2
wget http://forge.ocamlcore.org/frs/download.php/1096/batteries-2.0.tar.gz
wget http://tech.motion-twin.com/zip/xml-light-2.2.zip
cd ..
echo "================================================================================\n"

echo "Extracting..."
mkdir dependencies && cd dependencies
tar xzf ../archives/ocaml-4.00.1.tar.gz
tar xzf ../archives/findlib-1.3.3.tar.gz
tar xjf ../archives/camomile-0.8.3.tar.bz2
tar xzf ../archives/batteries-2.0.tar.gz
unzip -q ../archives/xml-light-2.2.zip
echo "================================================================================\n"

echo "Installing Ocaml 4.00.1"
cd ocaml-4.00.1
./configure
make world.opt
sudo make install
cd ..
echo "================================================================================\n"

echo "Installing findlib 1.3.3"
cd findlib-1.3.3
./configure
make all
make opt
sudo make install
cd ..
echo "================================================================================\n"

echo "Installing camomile 0.8.3"
cd camomile-0.8.3
./configure
make
sudo make install
cd ..
echo "================================================================================\n"

echo "Installing batteries 2.0"
cd batteries-2.0
make 
sudo make install
cd ..
echo "================================================================================\n"

echo "Installing xml-light 2.2"
cd xml-light
make 
make opt
echo 'version="2.2"' > META
echo 'archive(byte)="xml-light.cma"' >> META
echo 'archive(native)="xml-light.cmxa"' >> META
sudo ocamlfind install xml-light META *.cmx *.cmi *.mli *.a *.o *.cmxa
cd ..
cd ..
echo "================================================================================\n"

echo "Installing Cil"
cd cil
./configure
make
sudo make install
cd ..
echo "================================================================================\n"

echo "Building Goblint!!!"
cd analyzer
make
echo "================================================================================\n"
