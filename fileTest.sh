file=${1-"tests/file.c"}
result=${2-"html"}
mode=${3-"3"}
case "$mode" in
  1) args="--sets ana.activated[0][0] file"
  ;;
  2) args="--set ana.activated[0] '[\"base\", \"file\"]'"
  ;;
  3) args="--sets ana.activated[0][+] file"
  ;;
esac
cmd="./goblint --sets result $result $args $file"
echo $cmd
$cmd

# focuses Firefox and reloads current tab
if command -v xdotool >/dev/null 2>&1; then
  WID=`xdotool search --name "Mozilla Firefox" | head -1`
  xdotool windowactivate $WID
  #xdotool key F5
  # reload is done by add-on Auto Reload (reload result/* on change of report.html)
  # https://addons.mozilla.org/en-US/firefox/addon/auto-reload/?src=api
fi
