set -e
tmp="result/cil.c"
./goblint --enable justcil $@ | gcc -E -P - > $tmp
echo "./goblint ${@:1:$#-1} $tmp"
./goblint ${@:1:$#-1} $tmp
