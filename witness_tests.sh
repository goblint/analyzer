#! /usr/bin/bash
programs_dir="tests/regression/83-c2po/"
output_dir="result/witness_tests_output"
use_my_analysis="--set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts"

`mkdir $output_dir`

for entry in "$programs_dir"*
do
    filename=$(basename -- "$entry")
    `./goblint $use_my_analysis $entry --enable witness.yaml.enabled &> $output_dir/$filename.txt`
    `./goblint $use_my_analysis $entry --set witness.yaml.validate witness.yml &> $output_dir/${filename}_validation_c2po.txt`
    `./goblint $entry --set witness.yaml.validate witness.yml &> $output_dir/${filename}_validation_c2po.txt`
done
