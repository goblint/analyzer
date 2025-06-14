#!/usr/bin/env bash
set -e # exit immediately if a command fails
set -o pipefail # or all $? in pipe instead of returning exit code of the last command only

TARGET=src/goblint

opam_setup() {
  set -x
  opam init -y -a --bare $SANDBOXING # sandboxing is disabled in travis and docker
  opam update
  opam switch -y create . --deps-only --packages=ocaml-variants.4.14.2+options,ocaml-option-flambda --locked
}

rule() {
  case $1 in
    # new rules using dune
    clean)
      git clean -X -f
      dune clean
    ;; nat*)
      eval $(opam config env)
      dune build $TARGET.exe &&
      rm -f goblint &&
      cp _build/default/$TARGET.exe goblint
    ;; coverage)
      eval $(opam config env)
      dune build --instrument-with bisect_ppx $TARGET.exe &&
      rm -f goblint &&
      cp _build/default/$TARGET.exe goblint
    ;; release)
      eval $(opam config env)
      dune build --profile=release $TARGET.exe &&
      rm -f goblint &&
      cp _build/default/$TARGET.exe goblint
    ;; view)
      eval $(opam config env)
      dune build gobview
    ;; watch)
      eval $(opam config env)
      # dune build -w $TARGET.exe
      dune runtest --no-buffer --watch
    ;; privPrecCompare)
      eval $(opam config env)
      dune build src/privPrecCompare.exe &&
      rm -f privPrecCompare &&
      cp _build/default/src/privPrecCompare.exe privPrecCompare
    ;; apronPrecCompare)
      eval $(opam config env)
      dune build src/apronPrecCompare.exe &&
      rm -f apronPrecCompare &&
      cp _build/default/src/apronPrecCompare.exe apronPrecCompare
    ;; messagesCompare)
      eval $(opam config env)
      dune build src/messagesCompare.exe &&
      rm -f messagesCompare &&
      cp _build/default/src/messagesCompare.exe messagesCompare
    ;; byte)
      eval $(opam config env)
      dune build goblint.byte &&
      rm -f goblint.byte &&
      cp _build/default/goblint.byte goblint.byte

    # setup, dependencies
    ;; deps)
      eval $(opam config env)
      {
        opam install -y . --deps-only --locked --update-invariant &&
        opam upgrade -y $(opam list --pinned -s)
      } || {
        opam update
        opam pin remove -y $(opam list --pinned -s) || echo "No pins! All good...\n"
        opam install -y . --deps-only --locked --update-invariant
        opam upgrade -y $(opam list --pinned -s)
      }
    ;; setup)
      echo "Make sure you have the following installed: opam >= 2.0.0, git, patch, m4, autoconf, libgmp-dev, libmpfr-dev, pkg-config"
      echo "For the --html output you also need: javac, ant, dot (graphviz)"
      echo "For running the regression tests you also need: ruby, gem, curl"
      echo "For reference see ./Dockerfile or ./scripts/travis-ci.sh"
      opam_setup
    ;; dev)
      eval $(opam env)
      echo "Installing opam packages for test and doc..."
      opam install -y . --deps-only --locked --with-test --with-doc
      echo "Installing opam packages for development..."
      opam install -y ocaml-lsp-server ocp-indent
      # ocaml-lsp-server is needed for https://github.com/ocamllabs/vscode-ocaml-platform
      echo "Installing Pre-commit hook..."
      cd .git/hooks; ln -sf ../../scripts/hooks/pre-commit; cd -
      # Use `git commit -n` to temporarily bypass the hook if necessary.
      echo "Installing gem parallel (not needed for ./scripts/update_suite.rb -s)"
      sudo gem install parallel
    ;; headers)
      curl -L -O https://github.com/goblint/linux-headers/archive/master.tar.gz
      tar xf master.tar.gz && rm master.tar.gz
      rm -rf linux-headers && mv linux-headers-master linux-headers
      for n in $(compgen -c gcc- | sed 's/gcc-//'); do if [ $n != 5 ]; then cp -n linux-headers/include/linux/compiler-gcc{5,$n}.h; fi; done
    ;; npm)
      if test ! -e "webapp/package.json"; then
        git submodule update --init --recursive webapp
      fi
      cd webapp && npm install && npm start
    ;; jar)
      echo "Make sure you have the following installed: javac, ant, dot (from graphviz)"
      if test ! -e "g2html/build.xml"; then
        git submodule update --init --recursive g2html
      fi
      cd g2html && ant jar && cd .. &&
      cp g2html/g2html.jar .
    ;; setup_gobview )
      [[ -f gobview/gobview.opam ]] || git submodule update --init gobview
      opam install --deps-only --locked gobview/
    ;; install)
      eval $(opam config env)
      dune build @install
      dune install
    ;; uninstall)
      eval $(opam config env)
      dune uninstall
    ;; relocatable)
      PREFIX=relocatable
      # requires chrpath
      eval $(opam env)
      dune build @install
      dune install --relocatable --prefix $PREFIX
      # must replace absolute apron runpath to C library with relative
      chrpath -r '$ORIGIN/../share/apron/lib' $PREFIX/bin/goblint
      # remove goblint.lib ocaml library
      rm -r $PREFIX/lib
      # copy just necessary apron C libraries
      mkdir -p $PREFIX/share/apron/lib/
      cp _opam/share/apron/lib/libapron.so $PREFIX/share/apron/lib/
      cp _opam/share/apron/lib/liboctD.so $PREFIX/share/apron/lib/
      cp _opam/share/apron/lib/libboxD.so $PREFIX/share/apron/lib/
      cp _opam/share/apron/lib/libpolkaMPQ.so $PREFIX/share/apron/lib/

    # tests
    ;; test)
      eval $(opam env)
      dune runtest

    ;; sanitytest)
      ./scripts/update_suite.rb

    ;; *)
      echo "Unknown action '$1'. Try clean, native, byte, profile or doc.";;
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
