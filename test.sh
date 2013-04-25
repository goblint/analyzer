export OCAMLRUNPARAM=b
ana=${1-"file"}
result=${2-"html"}
file=${3-"tests/file.c"}
cmd="./goblint --sets ana.activated[0][+] $ana --sets result $result --set dbg.showtemps true --set dbg.debug true $file"
echo -e "$(tput setaf 6)$cmd$(tput sgr 0)"
$cmd
