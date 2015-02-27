#! /bin/sh
set -e

# generate the version file
scripts/set_version.sh

TARGET=src/goblint
FLAGS="-cflag -annot -tag bin_annot -X webapp -no-links -use-ocamlfind -j 8 -no-log -ocamlopt opt -cflag -g"
OCAMLBUILD=ocamlbuild

ocb() {
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)   rm -rf goblint goblint.byte goblint.ml arinc doclist.odocl src/config.ml $TARGET.ml;
             ocb -clean
             ;;
    opt | nat*)
             ocb -no-plugin $TARGET.native &&
             cp _build/$TARGET.native goblint
             ;;
    debug)   ocb -tag debug $TARGET.native &&
             cp _build/$TARGET.native goblint
             ;;
    bdebug)  ocb -tag debug $TARGET.d.byte &&
             cp _build/$TARGET.d.byte goblint.byte
             ;;
    # gprof (run only generates gmon.out). use: gprof goblint
    profile) ocb -tag profile $TARGET.p.native &&
             cp _build/$TARGET.p.native goblint
             ;;
    # gprof & ocamlprof (run also generates ocamlprof.dump). use: ocamlprof src/goblint.ml
    ocamlprof) ocb -ocamlopt ocamloptp $TARGET.p.native &&
             cp _build/$TARGET.p.native goblint
             ;;
    byte)    ocb $TARGET.byte &&
             cp _build/$TARGET.byte goblint.byte
             ;;
    all)     ocb $TARGET.native $TARGET.byte &&
             cp _build/$TARGET.native goblint &&
             cp _build/$TARGET.byte goblint.byte
             ;;
    doc*)    rm -rf doc;
             ls src/*/*/*.ml src/*/*.ml src/*.ml | sed 's/.*\/\(.*\)\.ml/\1/' > doclist.odocl;
             ocb -ocamldoc ocamldoc.opt -docflags -colorize-code,-keep-code doclist.docdir/index.html;
             rm doclist.odocl;
             ln -sf _build/doclist.docdir doc
             ;;
    tag*)    otags -vi `find src/ -iregex [^.]*\.mli?`;;
    arinc)   ocb src/mainarinc.native &&
             cp _build/src/mainarinc.native arinc
             ;;
    npm)     if test ! -e "webapp/package.json"; then
                git submodule update --init --recursive webapp
             fi
             cd webapp && npm install && npm start
             ;;
    jar)     if test ! -e "g2html/build.xml"; then
                git submodule update --init --recursive g2html
             fi
             cd g2html && ant jar && cd .. &&
             cp g2html/g2html.jar .
             ;;
    depend)  echo "No!";;
    setup)   echo "Make sure you have the following installed: opam >= 1.2.0, m4, patch, autoconf, git"
             opam init --comp=4.02.1
             opam update
             # opam switch 4.02.1
             eval `opam config env`
             opam install ocamlfind batteries xml-light
             # opam's cil is too old
             opam pin -y add cil "https://github.com/kerneis/cil.git"
             ;;
    dev)     opam install utop merlin ocp-indent ocp-index
             echo "Be sure to adjust your vim/emacs config!"
             ;;
    header*) wget http://www.ut.ee/~vesal/linux-headers.tbz
             tar xf linux-headers.tbz
             rm linux-headers.tbz
             ;;
    poly)    echo "open ApronDomain" >> $TARGET.ml
             echo "open Poly" >> $TARGET.ml
             ocb -no-plugin -package apron -package apron.polkaMPQ -package apron.octD $TARGET.native &&
             cp _build/$TARGET.native goblint
             ;;
    *)       echo "Unknown action '$1'. Try clean, opt, debug, profile, byte, or doc.";;
  esac; }

ls -1 src/*/*.ml | egrep -v "apronDomain|poly" | perl -pe 's/.*\/(.*)\.ml/open \u$1/g' > $TARGET.ml
ls -1 src/*/*/*.ml | perl -pe 's/.*\/(.*)\.ml/open \u$1/g' >> $TARGET.ml
echo "open Maingoblint" >> $TARGET.ml

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi

