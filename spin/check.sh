pml=${1-"deadlock.pml"}
max_depth=80000
max_steps=80000
rm -f $pml.trail
rm -f trail.txt
spin -a $pml &&
clang -o pan pan.c &&
./pan # checks for invalid end states (e.g. deadlocks), but only if there are no ltl claims inside! otherwise it selects the first claim!
# ./pan -E ignores invalid end states
# ./pan -a -N pw1 checks ltl claim pw1 (e.g. process 1 must not stay WAITING)

if [[ -e $pml.trail ]]; then
    spin -g -l -p -r -s -t -X $pml > trail.txt
    vim trail.txt
fi
