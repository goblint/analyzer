export OCAMLRUNPARAM=b
# file to analyze
file=${1-"tests/file.c"}
# spec file to use or "file" for file analysis
spec=${2-"tests/regression/18-file/file.optimistic.spec"}
debug=${debug-"true"}
if [ $spec == "file" ]; then
    ana="file --set ana.file.optimistic true"
else
    ana="spec --sets ana.spec.file ${spec}"
fi
cmd="./goblint --sets ana.activated[0][+] $ana --sets result html --enable colors --set dbg.showtemps true --set dbg.debug $debug $file"
echo -e "$(tput setaf 6)$cmd$(tput sgr 0)"
$cmd


# # focuses Firefox and reloads current tab
# if false && command -v xdotool >/dev/null 2>&1; then
#   WID=`xdotool search --name "Mozilla Firefox" | head -1`
#   xdotool windowactivate $WID
#   #xdotool key F5
#   # reload is done by add-on Auto Reload (reload result/* on change of report.html)
#   # https://addons.mozilla.org/en-US/firefox/addon/auto-reload/?src=api
# fi

