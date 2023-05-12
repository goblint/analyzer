#!/bin/bash

GOBLINTDIR="/home/julian/Entwicklung/Programm_Analyse/analyzer"
CONF="--conf conf/modular.json --set ana.modular.funs ['getndelim2']"
INPUT="cp_comb.c"

$GOBLINTDIR/goblint $CONF $INPUT -v &> out.txt
if [ $? -eq 3 ]; then
    grep Fixpoint out.txt >/dev/null 2>&1
else
    exit 5
fi