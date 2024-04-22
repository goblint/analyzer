#!/bin/bash

GOBLINTDIR="/home/julian/Entwicklung/goblint/analyzer"
CONF="--disable ana.opt.hashcons  --set dbg.timeout 900 --sem.unknown_function.spawn false --sem.unknown_function.call false --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'" --enable ana.sv-comp.functions  --enable allglobs --enable dbg.timing.enabled  --trace combine_env_modular_update --trace collect_targets_with_graph --set ana.modular.funs[+] "'knuth_morris_pratt_multibyte'" --enable ana.modular.only  --trace combine_env_modular_basic"
INPUT="cp_comb.c"
STRING=".buf[def_exc:Unknown int([-63,63])].buf[def_exc:Unknown int([-63,63])]"

echo 0 > out.txt
timeout 30 $GOBLINTDIR/goblint $CONF $INPUT -v &> out.txt
# if [ $? -eq 3 ]; then
grep  $STRING out.txt -F -q
# else
#     exit 5
# fi