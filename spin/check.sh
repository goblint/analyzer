pml=${1-"result/arinc.pml"}
claim=${2}
max_depth=800000
max_steps=800000
rm -f $pml.trail
rm -f trail.txt
spin -DPRIOS -a $pml &&
clang -Wno-parentheses-equality -o pan pan.c && # use -DNOLTL to exclude LTL claims
# ./pan # checks for invalid end states (e.g. deadlocks), but only if there are no ltl claims inside! otherwise it selects the first claim!
# ./pan -E  # ignores invalid end states
./pan -n -m$max_depth -a -N $claim # checks ltl claim pw (e.g. processes must not stay WAITING)
rm -f pan.* pan

if [[ -e $pml.trail ]]; then
    spin -g -l -p -r -s -t -X $pml > trail.txt
    vim trail.txt
fi
