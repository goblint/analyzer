# 1. install Java
# 2. install GraphViz http://www.graphviz.org/pub/graphviz/stable/windows/graphviz-2.36.msi
# 	(cygwinports (https://sourceware.org/cygwinports/) has GraphViz but the download is super slow)
# 3. cmd.exe: set PATH=%PATH%;C:\Program Files (x86)\Graphviz2.36\bin
# 4. run this script in Cygwin

function header() {
	echo
	echo
	echo "##### $1 #####"
}

function check(){
    hash $1 2>&- || (echo >&2 "$1 is needed but not installed! $2"; exit 1)
}

header "Checking Windows dependencies"
check java
installed_java=$?
check dot "\n\t1. Install GraphViz from here: http://www.graphviz.org/pub/graphviz/stable/windows/graphviz-2.36.msi\n\t2. Make sure dot.exe is in your path, i.e., start cmd.exe and do:\n\t\tset PATH=%PATH%;C:\Program Files (x86)\Graphviz2.36\bin"
installed_dot=$?

if [ $installed_java -ne 0 -o $installed_dot -ne 0 ]; then
	echo "Java and GraphViz are needed for the g2html output.";
	read -p "Continue anyway? " -n 1 -r
	echo    # (optional) move to a new line
	if [[ $REPLY =~ ^[Yy]$ ]]; then
	    echo "OK..."
	else
		exit
	fi
fi

header "Setup opam"
git clone https://github.com/ocaml/opam.git && cd opam
./configure && make && make install || exit 1
cd ..
opam init -a
eval `opam config env`

header "Install goblint's dependencies using opam"
opam install ocamlfind camomile batteries cil xml-light || exit 1

header "Get source and compile"
git clone https://github.com/goblint/analyzer.git && cd analyzer
make || exit 1

header "Upload binary"
read -p "Upload the compiled binary to goblint.in.tum.de? " -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]; then
    ./scripts/winupload.sh
fi