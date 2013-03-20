mode=${1-"png"}
file=${2-"file"}
dst=graph
viewcmd=gpicview
mkdir -p ${dst}
cp ${file}.dot ${dst}
cd ${dst}
case "$mode" in
  png) dot -Tpng -o${file}.png ${file}.dot;
       pkill ${viewcmd};
       ${viewcmd} ${file}.png &
  ;;
  pdf) rm -f ${file}.tex;
       dot -Txdot ${file}.dot | dot2tex > ${file}.tex;
       pdflatex ${file}.tex
  ;;
esac
cd ..
