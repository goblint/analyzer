spin -a ${1-"airbus.pml"}
cc -o pan pan.c
rm -f pan.dot pan.svg
./pan -D | dot | gvpack -array_u > pan.dot
dot -Tsvg pan.dot > pan.svg
open -a Safari pan.svg