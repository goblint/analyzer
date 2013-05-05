export OCAMLRUNPARAM=b
ana=${1-"file"}
result=${2-"html"}
file=${3-"tests/file.c"}
if [ $ana == "spec" ]; then
    ana="$ana --sets spec.file src/spec/file.spec"
fi
dbg=${goblintdebug-"false"}
cmd="./goblint --sets ana.activated[0][+] $ana --sets result $result --set dbg.showtemps true --set dbg.debug $dbg $file"
echo -e "$(tput setaf 6)$cmd$(tput sgr 0)"
$cmd
