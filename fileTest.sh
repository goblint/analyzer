result=${2-"html"}
case "$1" in
  1) args="--sets ana.activated[0][0] file"
  ;;
  2) args="--set ana.activated[0] '[\"base\", \"file\"]'"
  ;;
  3) args="--sets ana.activated[0][+] file"
  ;;
esac
cmd="./goblint --sets result $result $args tests/file.c"
echo $cmd
$cmd
