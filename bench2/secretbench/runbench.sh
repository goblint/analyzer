#!/bin/sh

ulimit -Ss 49152
export OCAMLRUNPARAM=s=32M,i=32M,o=150

CMD="../../goblint"



for f in `ls *.c` 
do
  for c in "widen1.json slr1.json	slr2.json	slr3.json	slr4.json"
  do
    echo "";
    echo "";
    echo ${f};
    echo ${CMD} --conf ${c} ${f};
    ${CMD} --conf ${c} ${f};
  done
done

#mv result.txt result.sens.widen.txt
