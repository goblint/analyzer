export OCAMLRUNPARAM=b
# file to analyze
file=${1-"tests/file.c"}
# analysis to run or spec file
ana=${2-"tests/regression/18-file/file.optimistic.spec"}
debug=${debug-"true"}
if [ $ana == "file" ]; then
    ana="file"
    opt="--set ana.file.optimistic true"
else
    spec=$ana
    ana="spec"
    opt="--set ana.spec.file $spec"
fi
cmd="./goblint --set ana.activated[0][+] $ana $opt --html --set dbg.debug $debug $file"
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
