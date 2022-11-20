#!/usr/bin/env bash
set -e # exit immediately if a command fails
set -o pipefail # or all $? in pipe instead of returning exit code of the last command only

TARGET=src/goblint

opam_setup() {
  set -x
  opam init -y -a --bare $SANDBOXING # sandboxing is disabled in travis and docker
  opam update
  opam switch -y create . --deps-only ocaml-variants.4.14.0+options ocaml-option-flambda --locked
}

opam_setup_flambda() {
  set -x
  opam init -y -a --bare $SANDBOXING # sandboxing is disabled in travis and docker
  opam update
  # Note: the `--update-invariant` option is needed for replacement of the previous ocaml compiler switch invariant of Goblint
  opam switch -y create . --deps-only ocaml-variants.4.14.0+options ocaml-option-flambda --locked --update-invariant
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
      rm -f goblint &&
      cp _build/default/$TARGET.exe goblint
    ;; release)
      eval $(opam config env)
      dune build --profile=release $TARGET.exe &&
      rm -f goblint &&
      cp _build/default/$TARGET.exe goblint
    # alternatives to .exe: .bc (bytecode), .bc.js (js_of_ocaml), see https://dune.readthedocs.io/en/stable/dune-files.html#executable
    ;; js) # https://dune.readthedocs.io/en/stable/jsoo.html
      dune build $TARGET.bc.js &&
      node _build/default/$TARGET.bc.js
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
    # ;; tag*)
    #   otags -vi `find src/ -iregex [^.]*\.mli?`

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
    ;; setup-flambda)
      echo "Make sure you have the following installed: opam >= 2.0.0, git, patch, m4, autoconf, libgmp-dev, libmpfr-dev, pkg-config"
      echo "For the --html output you also need: javac, ant, dot (graphviz)"
      echo "For running the regression tests you also need: ruby, gem, curl"
      echo "For reference see ./Dockerfile or ./scripts/travis-ci.sh"
      opam_setup_flambda
    ;; dev)
      eval $(opam env)
      echo "Installing opam packages for development..."
      opam install -y utop ocaml-lsp-server ocp-indent ocamlformat ounit2
      # ocaml-lsp-server is needed for https://github.com/ocamllabs/vscode-ocaml-platform
      echo "Be sure to adjust your vim/emacs config!"
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
    ;; setup_gobview )
      [[ -f gobview/gobview.opam ]] || git submodule update --init gobview
      opam install --deps-only --locked gobview/
    # ;; watch)
    #   fswatch --event Updated -e $TARGET.ml src/ | xargs -n1 -I{} make
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

    # tests, CI
    ;; test)
      chmod -R +w ./tests/ # dune runtest normally has everything read-only, but update_suite wants to write a lot of things
      mkdir -p ./tests/suite_result
      ./scripts/update_suite.rb # run regression tests
    ;; testci)
      ruby scripts/update_suite.rb -s -d # -s: run tests sequentially instead of in parallel such that output is not scrambled, -d shows some stats?
    ;; travis) # run a travis docker container with the files tracked by git - intended to debug setup problems on travis-ci.com
      echo "run ./scripts/travis-ci.sh to setup ocaml"
      # echo "bind-mount cwd: beware that cwd of host can be modified and IO is very slow!"
      # docker run -it -u travis -v $(pwd):$(pwd):delegated -w $(pwd) travisci/ci-garnet:packer-1515445631-7dfb2e1 bash
      echo "copy cwd w/o git-ignored files: changes in container won't affect host's cwd."
      # cp cwd (with .git, _opam, _build): 1m51s, cp ls-files: 0.5s
      docker run -it -u travis -v `pwd`:/analyzer:ro,delegated -w /home/travis travisci/ci-garnet:packer-1515445631-7dfb2e1 bash -c 'cd /analyzer; mkdir ~/a; cp --parents $(git ls-files) ~/a; cd ~/a; bash'
    ;; server)
      rsync -avz --delete --exclude='/.git' --exclude='server.sh' --exclude-from="$(git ls-files --exclude-standard -oi --directory > /tmp/excludes; echo /tmp/excludes)" . serverseidl6.informatik.tu-muenchen.de:~/analyzer2
      ssh serverseidl6.informatik.tu-muenchen.de 'cd ~/analyzer2; make nat && make test'

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
