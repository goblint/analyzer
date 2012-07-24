#! /bin/sh
set -e

# generate the version file
scripts/set_version.sh

TARGET=src/maingoblint
FLAGS="-cflags -for-pack,Goblint -no-links -use-ocamlfind -j 8 -no-log"
OCAMLBUILD=ocamlbuild

ocb() {
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)   rm -rf goblint goblint.byte;
             ocb -clean
             ;;
    opt | nat*)
             ocb $TARGET.native &&
             mv _build/src/maingoblint.native goblint
             ;;
    debug)   ocb -tag debug $TARGET.native &&
             mv _build/src/maingoblint.native goblint
             ;;
    profile) ocb -tag profile $TARGET.native &&
             mv _build/src/maingoblint.native goblint
             ;;
    byte)    ocb $TARGET.byte &&
             mv _build/src/maingoblint.byte goblint.byte
             ;;
    plug*)   FLAGS="-cflags -I,/usr/lib/frama-c -lflags -I,/usr/lib/frama-c $FLAGS";
             ocb src/frama.cmxs && sudo mv _build/src/frama.cmxs /usr/lib/frama-c/plugins/Goblint.cmxs
             ;;
    all)     ocb $TARGET.native $TARGET.byte &&
             mv _build/src/maingoblint.native goblint &&
             mv _build/src/maingoblint.byte goblint.byte
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

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift 
  done
fi
