#!/bin/sh

export OCAMLRUNPARAM=s=4M,i=32M,o=150

FILES="compress.c adpcm.c fft1.c lms.c nsichneu.c bs.c bsort100.c cnt.c cover.c crc.c duff.c edn.c expint.c fac.c fdct.c fibcall.c fir.c insertsort.c janne_complex.c jfdctint.c lcdnum.c  loop3.c ludcmp.c matmult.c minmax.c minver.c ns.c qsort-exam.c qurt.c select.c sqrt.c statemate.c wideningPaper/*"


CMD="../../goblint"


for c in "two1.json 12.json 23.json 34.json"
do
  for f in ${FILES} 
  do
    echo "" 
    echo "" 
    echo ${f};
    echo ${CMD} --conf ${c} ${f};
  	${CMD} --conf ${c} ${f};
  done
done