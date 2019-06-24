#! /bin/bash
set -e

TARGET=src/goblint
FLAGS="-cflag -annot -tag bin_annot -X webapp -no-links -use-ocamlfind -j 8 -no-log -ocamlopt opt -cflag -g"
OCAMLBUILD=ocamlbuild
EXCLUDE="_build|goblint.ml|apronDomain|poly"

ocb() {
  command -v opam >/dev/null 2>&1 && eval $(opam config env)
  scripts/set_version.sh # generate the version file
  ls -1 src/**/*.ml | egrep -v $EXCLUDE | perl -pe 's/.*\/(.*)\.ml/open \u$1/g' > $TARGET.ml
  echo "open Maingoblint" >> $TARGET.ml
  $OCAMLBUILD $FLAGS $*
}

opam_setup() {
  set -x
  opam init -y -a --bare $SANDBOXING # sandboxing is disabled in travis and docker
  opam switch -y create ./ --deps-only 4.07.1
  # opam install camlp4 mongo # camlp4 needed for mongo
}

rule() {
  case $1 in
    clean)
      rm -rf goblint goblint.byte arinc doclist.odocl $TARGET.ml;
      ocb -clean
    ;; opt | nat*)
      ocb -no-plugin $TARGET.native &&
      cp _build/$TARGET.native goblint
    ;; debug)
      ocb -tag debug $TARGET.d.byte &&
      cp _build/$TARGET.d.byte goblint.byte
    ;; warn)
      # be pedantic and show all warnings
      $OCAMLBUILD $FLAGS -no-plugin -cflags "-w +a" $TARGET.native && # copied b/c passing a quoted argument to a function does not work
      cp _build/$TARGET.native goblint
    ;; profile)
      # gprof (run only generates gmon.out). use: gprof goblint
      ocb -tag profile $TARGET.p.native &&
      cp _build/$TARGET.p.native goblint
    ;; ocamlprof)
      # gprof & ocamlprof (run also generates ocamlprof.dump). use: ocamlprof src/goblint.ml
      ocb -ocamlopt ocamloptp $TARGET.p.native &&
      cp _build/$TARGET.p.native goblint
    ;; doc*)
      rm -rf doc;
      ls src/**/*.ml | egrep -v $EXCLUDE  | sed 's/.*\/\(.*\)\.ml/\1/' > doclist.odocl;
      ocb -ocamldoc ocamldoc -docflags -charset,utf-8,-colorize-code,-keep-code doclist.docdir/index.html;
      rm doclist.odocl;
      ln -sf _build/doclist.docdir doc
    ;; tag*)
      otags -vi `find src/ -iregex [^.]*\.mli?`
    ;; poly)
      echo "open ApronDomain" >> $TARGET.ml
      echo "open Poly" >> $TARGET.ml
      ocb -no-plugin -package apron -package apron.polkaMPQ -package apron.octD $TARGET.native &&
      cp _build/$TARGET.native goblint
    ;; arinc)
      ocb src/mainarinc.native &&
      cp _build/src/mainarinc.native arinc
    ;; npm)
      if test ! -e "webapp/package.json"; then
        git submodule update --init --recursive webapp
      fi
      cd webapp && npm install && npm start
    ;; jar)
      echo "Make sure you have the following installed: javac, ant"
      if test ! -e "g2html/build.xml"; then
        git submodule update --init --recursive g2html
      fi
      cd g2html && ant jar && cd .. &&
      cp g2html/g2html.jar .
    ;; deps)
      opam install -y . --deps-only
    ;; setup)
      echo "Make sure you have the following installed: opam >= 1.2.2, m4, patch, autoconf, git"
      echo "For the --html output you also need: javac, ant, dot (graphviz)"
      echo "For running the regression tests you also need: ruby, gem, curl"
      opam_setup
    ;; dev)
      echo "Installing opam packages for development..."
      opam install utop merlin ocp-indent ounit
      echo "Be sure to adjust your vim/emacs config!"
      echo "Installing Pre-commit hook..."
      cd .git/hooks; ln -s ../../scripts/hooks/pre-commit; cd -
      echo "Installing gem parallel (not needed for ./scripts/update_suite.rb -s)"
      sudo gem install parallel
    ;; lock)
      opam lock .
    ;; watch)
      fswatch --event Updated -e $TARGET.ml src/ | xargs -n1 -I{} make
    ;; headers)
      curl -L -O https://github.com/goblint/linux-headers/archive/master.tar.gz
      tar xf master.tar.gz && rm master.tar.gz
      rm -rf linux-headers && mv linux-headers-master linux-headers
      cp linux-headers/include/linux/compiler-gcc5.h linux-headers/include/linux/compiler-gcc6.h
      cp linux-headers/include/linux/compiler-gcc5.h linux-headers/include/linux/compiler-gcc7.h
      cp linux-headers/include/linux/compiler-gcc5.h linux-headers/include/linux/compiler-gcc8.h
      cp linux-headers/include/linux/compiler-gcc5.h linux-headers/include/linux/compiler-gcc9.h
    ;; test)
      ./scripts/update_suite.rb # run regression tests
    ;; testci)
      ruby scripts/update_suite.rb -s -d
    ;; travis)
      echo "run ./scripts/travis-ci.sh to setup ocaml"
      # echo "bind-mount cwd: beware that cwd of host can be modified and IO is very slow!"
      # docker run -it -u travis -v $(pwd):$(pwd):delegated -w $(pwd) travisci/ci-garnet:packer-1515445631-7dfb2e1 bash
      echo "copy cwd w/o git-ignored files: changes in container won't affect host's cwd."
      # cp cwd (with .git, _opam, _build): 1m51s, cp ls-files: 0.5s
      docker run -it -u travis -v `pwd`:/analyzer:ro,delegated -w /home/travis travisci/ci-garnet:packer-1515445631-7dfb2e1 bash -c 'cd /analyzer; mkdir ~/a; cp --parents $(git ls-files) ~/a; cd ~/a; bash'
    ;; unit)
      ocamlbuild -use-ocamlfind unittest/mainTest.native && ./mainTest.native
    ;; server)
      rsync -avz --delete --exclude='/.git' --exclude='server.sh' --exclude-from="$(git ls-files --exclude-standard -oi --directory > /tmp/excludes; echo /tmp/excludes)" . serverseidl6.informatik.tu-muenchen.de:~/analyzer2
      ssh serverseidl6.informatik.tu-muenchen.de 'cd ~/analyzer2; make nat && make test'
    ;; *)
      echo "Unknown action '$1'. Try clean, opt, debug, profile, byte, or doc.";;
  esac;
}

if [ $# -eq 0 ]; then
  rule nat
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi
