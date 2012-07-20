#! /bin/sh
set -e

# generate the version file
scripts/set_version.sh

TARGET=src/maingoblint
FLAGS="-no-links -use-ocamlfind -lib xml-light -j 8"
OCAMLBUILD=ocamlbuild

ocb() {
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)  rm -rf goblint goblint.byte;
            ocb -clean
            ;;
    native) ocb $TARGET.native;
            ln -sf _build/src/maingoblint.native goblint
            ;;
    byte)   ocb $TARGET.byte;
            ln -sf _build/src/maingoblint.byte goblint.byte
            ;;
    all)    ocb $TARGET.native $TARGET.byte;
            ln -sf _build/src/maingoblint.native goblint;
            ln -sf _build/src/maingoblint.byte goblint.byte
            ;;
    doc)    ocb doclist.docdir/index.html;
            rm -rf docs;
            ln -sf _build/doclist.docdir/ docs;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
esac; }

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift 
  done
fi