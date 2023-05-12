#!/bin/bash

GOBLINTDIR="/home/julian/Entwicklung/Programm_Analyse/analyzer"
CONF="--conf conf/modular.json --set ana.modular.funs ['getndelim2']"
INPUT="cp_comb.c"

echo $GOBLINTDIR/goblint $CONF $INPUT -v
timeout 20 $GOBLINTDIR/goblint $CONF $INPUT -v &> out.txt
if [[ $? -eq 2 || $? -eq 124 ]]; then
    grep "max updates.*tmp___1" out.txt >/dev/null 2>&1
else
    exit 5
fi

