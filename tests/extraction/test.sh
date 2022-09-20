#!/usr/bin/env bash
mkdir -p run
cp -r ../../spin run/spin
cd run

../../../goblint --set ana.activated[+] extract-pthread ../00-sanity.c && spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a &> out.txt
output=$(cat out.txt | grep pthread.pml.trail)
if [ "$output" ]
then
    echo "Unexpected verdict: Starving"
    exit 1
fi

../../../goblint --set ana.activated[+] extract-pthread ../01-base.c && spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a &> out.txt
output=$(cat out.txt | grep pthread.pml.trail)
if  [ -z "$output" ]
then
    echo "Unexpected verdict: Non-Starving"
    exit 2
fi

../../../goblint --set ana.activated[+] extract-pthread ../02-starve.c && spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a &> out.txt
output=$(cat out.txt | grep pthread.pml.trail)
if [ -z "$output" ]
then
    echo "Unexpected verdict: Non-Starving"
    exit 3
fi
