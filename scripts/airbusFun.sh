set -e
#~/analyzer/goblint --sets result html --sets "ana.activated[0][+]" arinc --enable colors --enable dbg.debug --trace arinc --sets "mainfun[0]" "F787" /home/ralf/mbat/airbus/arinc_example/monit.c
export OCAMLRUNPARAM=b
export scrambled=true
pushd $(dirname $0) # cwd to where this script is
cp -f ~/analyzer/goblint .
mkdir -p result
# cp -f ~/analyzer/spin/arinc.base.pml result
# --trace set --tracevars G101 --trace theta --trace get
trace="result/trace.fun.txt"
dbg="--enable colors --enable dbg.debug --enable dbg.verbose"
(
set -o pipefail # otherwise the following will always succeed because of tee, even if goblint has a non-zero exit status
/bin/time -v ./goblint --conf ab.conf --set "ana.activated[0]" "['base','arincFun']" $dbg --enable ana.arinc.export --enable ana.arinc.assume_success --trace arinc unrolled_monit.c 2>&1 | tee $trace
)
dot="result/arinc.fun.dot"
wc=$(wc -l < $dot)
echo "$dot has $wc lines!" | tee -a $trace
fdp -Tpng -O $dot
gnome-open ${dot}.png
pushd result
echo "Generating SPIN Verifier from Promela Code"
if [ ! -f arinc.base.pml ]; then
    cp ~/analyzer/spin/arinc.base.pml .
fi
spin -DPRIOS -a arinc.fun.pml
clang -DVECTORSZ=5000 -o pan pan.c # complained that VECTORSZ should be > 1280
echo "Verify! If there are errors, this will generate a file arinc.fun.pml.trail"
./pan -a || (echo "Verification failed! Do simulation guided by trail."; spin -g -l -p -r -s -t -X -u10000 arinc.fun.pml)
popd # result
popd # script dir
unset scrambled
