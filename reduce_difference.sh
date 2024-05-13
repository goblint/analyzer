#!/bin/bash
grep "void (\*argmatch_die)(void)  =    & __argmatch_die;" reduce.c
if [ $? -eq 1 ]; then
    exit 17
fi
grep "static void __argmatch_die(void)" reduce.c
if [ $? -eq 1 ]; then
    exit 17
fi
grep "usage(1)" reduce.c
if [ $? -eq 1 ]; then
    exit 17
fi


/home/michael/Documents/goblint-cil/analyzer/goblint-2  --conf /home/michael/Documents/goblint-cil/analyzer/conf/traces.json --set dbg.timeout 900 --set ana.base.privatization protection 1.c --enable allglobs --enable dbg.timing.enabled --enable warn.debug -v --sets exp.priv-prec-dump level-ip.protection.prec
/home/michael/Documents/goblint-cil/analyzer/goblint-2  --conf /home/michael/Documents/goblint-cil/analyzer/conf/traces.json --set dbg.timeout 900 --set ana.base.privatization write 1.c --enable allglobs --enable dbg.timing.enabled --enable warn.debug -v --sets exp.priv-prec-dump level-ip.write.prec
/home/michael/Documents/goblint-cil/analyzer/privPrecCompare level-ip.protection.prec level-ip.write.prec  2> details.txt 1> res.txt
if [ $? -eq 0 ]; then
    grep "protection more precise than write" res.txt >/dev/null 2>&1
else
    exit 5
fi
