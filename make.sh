#! /bin/sh
set -e

# generate the version file
scripts/set_version.sh

TARGET=src/maingoblint
FLAGS="-no-links -use-ocamlfind -j 8 -no-log"
OCAMLBUILD=ocamlbuild

ocb() {
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)   rm -rf goblint goblint.byte;
             ocb -clean
             ;;
    native)  ocb $TARGET.native;
             ln -sf _build/src/maingoblint.native goblint
             ;;
    debug)   ocb -tag debug $TARGET.native;
             ln -sf _build/src/maingoblint.native goblint
             ;;
    profile) ocb -tag profile $TARGET.native;
             ln -sf _build/src/maingoblint.native goblint
             ;;
    byte)    ocb $TARGET.byte;
             ln -sf _build/src/maingoblint.byte goblint.byte
             ;;
    all)     ocb $TARGET.native $TARGET.byte;
             ln -sf _build/src/maingoblint.native goblint;
             ln -sf _build/src/maingoblint.byte goblint.byte
             ;;
    doc)     ls src/*/*.ml src/*.ml | sed 's/.*\/\(.*\)\.ml/\1/' > doclist.odocl;
             ocb doclist.docdir/index.html;
             ln -sf _build/doclist.docdir doc;;
    depend)  echo "No!";;
    *)       echo "Unknown action '$1'. Try clean, native, debug, profile, byte, or doc.";;
  esac; }

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift 
  done
fi