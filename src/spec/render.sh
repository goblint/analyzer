# command -v ls >&- || {echo >&2 bla; exit 1;}
function check(){
    set -e # needed to exit script from function
    hash $1 2>&- || (echo >&2 "$1 is needed but not installed! $2"; exit 1;)
    set +e # do not exit shell if some command fails (default)
}
check dot
mode=${1-"png"}
file=${2-"file"}
dst=graph
viewcmd=gpicview

mkdir -p ${dst}
cp ${file}.dot ${dst}
cd ${dst}
trap 'cd ..' EXIT # leave dst again on exit
case "$mode" in
  png) dot -Tpng -o${file}.png ${file}.dot;
       check ${viewcmd} "Please edit viewcmd accordingly."
       pkill ${viewcmd};
       ${viewcmd} ${file}.png &
  ;;
  pdf) rm -f ${file}.tex;
       check dot2tex
       dot -Txdot ${file}.dot | dot2tex > ${file}.tex;
       check pdflatex
       pdflatex ${file}.tex
       echo  "generated $dst/$file.pdf"
  ;;
esac

