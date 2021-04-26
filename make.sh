#! /bin/bash
set -e # exit immediately if a command fails
set -o pipefail # or all $? in pipe instead of returning exit code of the last command only

TARGET=src/goblint

gen() { # generate configuration files and goblint.ml which opens all modules in src/ such that they will be linked and executed without the need to be referenced somewhere else
  scripts/set_version.sh # generate the version file
  echo '[@@@ocaml.warning "-33"]' > $TARGET.ml # disable warning 'Unused open statement.'
  ls -1 src/**/*.ml | egrep -v "goblint.ml|violationZ3" | perl -pe 's/.*\/(.*)\.ml/open \u$1/g' >> $TARGET.ml
  echo "let _ = at_exit Maingoblint.main" >> $TARGET.ml
}

opam_setup() {
  set -x
  opam init -y -a --bare $SANDBOXING # sandboxing is disabled in travis and docker
  opam update
  opam switch -y create . --deps-only ocaml-base-compiler.4.11.1 --locked
  # opam install camlp4 mongo # camlp4 needed for mongo
}

# deprecated, use dune which is much faster
OCBFLAGS="-cflag -annot -tag bin_annot -X webapp -no-links -use-ocamlfind -j 8 -no-log -ocamlopt opt -cflag -g"
ocb() {
  command -v opam >/dev/null 2>&1 && eval $(opam config env)
  gen
  ocamlbuild $OCBFLAGS $*
}

rule() {
  case $1 in
    # new rules using dune
    clean)
      git clean -X -f
      dune clean
    ;; gen) gen
    ;; nat*)
      eval $(opam config env)
      dune build $TARGET.exe &&
      cp _build/default/$TARGET.exe goblint
    ;; release)
      eval $(opam config env)
      dune build --profile release $TARGET.exe &&
      cp _build/default/$TARGET.exe goblint
    # alternatives to .exe: .bc (bytecode), .bc.js (js_of_ocaml), see https://dune.readthedocs.io/en/stable/dune-files.html#executable
    ;; js) # https://dune.readthedocs.io/en/stable/jsoo.html
      dune build $TARGET.bc.js &&
      node _build/default/$TARGET.bc.js
    ;; watch)
      eval $(opam config env)
      # dune build -w $TARGET.exe
      dune runtest --no-buffer --watch
    ;; domaintest)
      eval $(opam config env)
      dune build src/maindomaintest.exe &&
      cp _build/default/src/maindomaintest.exe goblint.domaintest
    # old rules using ocamlbuild
    ;; ocbnat*)
      ocb -no-plugin $TARGET.native &&
      cp _build/$TARGET.native goblint
    ;; debug)
      ocb -tag debug $TARGET.d.byte &&
      cp _build/$TARGET.d.byte goblint.byte
    ;; profile)
      # gprof (run only generates gmon.out). use: gprof goblint
      ocb -tag profile $TARGET.p.native &&
      cp _build/$TARGET.p.native goblint
    ;; ocamlprof)
      # gprof & ocamlprof (run also generates ocamlprof.dump). use: ocamlprof src/goblint.ml
      ocb -ocamlopt ocamloptp $TARGET.p.native &&
      cp _build/$TARGET.p.native goblint
    # ;; docs)
    #   rm -rf doc;
    #   ls src/**/*.ml | egrep -v $EXCLUDE  | sed 's/.*\/\(.*\)\.ml/\1/' > doclist.odocl;
    #   ocb -ocamldoc ocamldoc -docflags -charset,utf-8,-colorize-code,-keep-code doclist.docdir/index.html;
    #   rm doclist.odocl;
    #   ln -sf _build/doclist.docdir doc
    # ;; tag*)
    #   otags -vi `find src/ -iregex [^.]*\.mli?`
    ;; poly)
      echo "open ApronDomain" >> $TARGET.ml
      echo "open Poly" >> $TARGET.ml
    ;; octApron)
      echo "open OctApronDomain" >> $TARGET.ml
      echo "open OctApron" >> $TARGET.ml
      ocb -no-plugin -package apron -package apron.polkaMPQ -package apron.octD $TARGET.native &&
      cp _build/$TARGET.native goblint
    ;; arinc)
      ocb src/mainarinc.native &&
      cp _build/src/mainarinc.native arinc

    # setup, dependencies
    ;; deps)
      opam update; opam install -y . --deps-only --locked --unlock-base
    ;; setup)
      echo "Make sure you have the following installed: opam >= 2.0.0, git, patch, m4, autoconf, libgmp-dev, libmpfr-dev"
      echo "For the --html output you also need: javac, ant, dot (graphviz)"
      echo "For running the regression tests you also need: ruby, gem, curl"
      echo "For reference see ./Dockerfile or ./scripts/travis-ci.sh"
      opam_setup
    ;; dev)
      echo "Installing opam packages for development..."
      opam install utop ocaml-lsp-server ocp-indent ocamlformat ounit2
      # ocaml-lsp-server is needed for https://github.com/ocamllabs/vscode-ocaml-platform
      echo "Be sure to adjust your vim/emacs config!"
      echo "Installing Pre-commit hook..."
      cd .git/hooks; ln -s ../../scripts/hooks/pre-commit; cd -
      echo "Installing gem parallel (not needed for ./scripts/update_suite.rb -s)"
      sudo gem install parallel
    ;; headers)
      curl -L -O https://github.com/goblint/linux-headers/archive/master.tar.gz
      tar xf master.tar.gz && rm master.tar.gz
      rm -rf linux-headers && mv linux-headers-master linux-headers
      for n in $(compgen -c gcc- | sed 's/gcc-//'); do if [ $n != 5 ]; then cp -n linux-headers/include/linux/compiler-gcc{5,$n}.h; fi; done
    ;; lock)
      opam lock
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
    # ;; watch)
    #   fswatch --event Updated -e $TARGET.ml src/ | xargs -n1 -I{} make

    # tests, CI
    ;; test)
      ./scripts/update_suite.rb # run regression tests
    ;; unit)
      ocamlbuild -use-ocamlfind unittest/mainTest.native && ./mainTest.native
    ;; testci)
      ruby scripts/update_suite.rb -s -d # -s: run tests sequentially instead of in parallel such that output is not scrambled, -d shows some stats?
    ;; travis) # run a travis docker container with the files tracked by git - intended to debug setup problems on travis-ci.com
      echo "run ./scripts/travis-ci.sh to setup ocaml"
      # echo "bind-mount cwd: beware that cwd of host can be modified and IO is very slow!"
      # docker run -it -u travis -v $(pwd):$(pwd):delegated -w $(pwd) travisci/ci-garnet:packer-1515445631-7dfb2e1 bash
      echo "copy cwd w/o git-ignored files: changes in container won't affect host's cwd."
      # cp cwd (with .git, _opam, _build): 1m51s, cp ls-files: 0.5s
      docker run -it -u travis -v `pwd`:/analyzer:ro,delegated -w /home/travis travisci/ci-garnet:packer-1515445631-7dfb2e1 bash -c 'cd /analyzer; mkdir ~/a; cp --parents $(git ls-files) ~/a; cd ~/a; bash'
    ;; docker) # build and run a docker image
      docker build --pull -t goblint . | ts -i
      docker run -it goblint bash
    ;; server)
      rsync -avz --delete --exclude='/.git' --exclude='server.sh' --exclude-from="$(git ls-files --exclude-standard -oi --directory > /tmp/excludes; echo /tmp/excludes)" . serverseidl6.informatik.tu-muenchen.de:~/analyzer2
      ssh serverseidl6.informatik.tu-muenchen.de 'cd ~/analyzer2; make nat && make test'

    ;; *)
      echo "Unknown action '$1'. Try clean, opt, debug, profile, byte, or doc.";;
  esac;
}

if [ $# -eq 0 ]; then
  rule native
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi
