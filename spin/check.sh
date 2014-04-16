pml=${1-"arinc.pml"}
max_depth=80000
max_steps=80000
rm -f $pml.trail
rm -f trail.txt
spin -a $pml &&
gcc -DSAFETY  -o pan pan.c &&
./pan -m$max_depth -X

if [[ -e $pml.trail ]]; then
    spin -g -l -p -r -s -t -X -u$max_steps $pml > trail.txt
    vim trail.txt
fi
