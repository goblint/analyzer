export OCAMLRUNPARAM=b
ana=${1-"file"}
file=${2-"tests/file.c"}
result=${3-"html"}
if [ $ana == "spec" ]; then
    ana="$ana --sets spec.file src/spec/file.spec"
fi
dbg=${goblintdebug-"false"}
cmd="./goblint --sets ana.activated[0][+] $ana --sets result $result --set dbg.showtemps true --set dbg.debug $dbg $file"
echo -e "$(tput setaf 6)$cmd$(tput sgr 0)"
$cmd
