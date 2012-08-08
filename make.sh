#! /bin/sh
set -e

# generate the version file
scripts/set_version.sh

TARGET=src/goblint
FLAGS="-no-links -use-ocamlfind -j 8 -no-log"
OCAMLBUILD=ocamlbuild

ocb() {
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)   rm -rf goblint goblint.byte goblint.ml doclist.odocl;
             ocb -clean
             ;;
    opt | nat*)
             ocb $TARGET.native &&
             mv _build/src/goblint.native goblint
             ;;
    debug)   ocb -tag debug $TARGET.native &&
             mv _build/src/goblint.native goblint
             ;;
    profile) ocb -tag profile $TARGET.native &&
             mv _build/src/goblint.native goblint
             ;;
    byte)    ocb $TARGET.byte &&
             mv _build/src/goblint.byte goblint.byte
             ;;
    all)     ocb $TARGET.native $TARGET.byte &&
             mv _build/src/goblint.native goblint &&
             mv _build/src/goblint.byte goblint.byte
             ;;
    doc*)    rm -rf doc;
             ls src/*/*.ml src/*.ml | sed 's/.*\/\(.*\)\.ml/\1/' > doclist.odocl;
             ocb doclist.docdir/index.html;
             rm doclist.odocl;
             ln -sf _build/doclist.docdir doc
             ;;
    depend)  echo "No!";;
    *)       echo "Unknown action '$1'. Try clean, native, debug, profile, byte, or doc.";;
  esac; }

ls -1 src/analyses/*.ml | perl -pe 's/.*\/(.*)\.ml/open \u$1/g' > src/goblint.ml
echo "open Maingoblint" >> src/goblint.ml

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift 
  done
fi

rm -f src/goblint.ml
