spin -a ${1-"airbus.pml"}
cc -o pan pan.c
./pan -D | dot | gvpack -array_u >! pan.dot
zgrv pan.dot
