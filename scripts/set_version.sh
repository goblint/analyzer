#!/bin/bash
VERSION=$(git describe --tags --dirty 2> /dev/null | sed s/^v//)
CILVERSION=$(git --git-dir=../cil/.git describe --tags 2> /dev/null | sed s/^cil-//)

if [ ! -f src/version.ml ]; then
  {
    echo "let goblint = \"unknown\""
    echo "let cil = \"unknown\""
  } >> src/version.ml
fi

if [ ! -f src/config.ml ]; then
  cpp="cpp"
  # if we are on OS X and cpp is the Apple LLVM version, we should look for some Homebrew gcc cpp-4.9 (or other version)
  case $OSTYPE in darwin*) if compgen -c | grep -q "^cpp-"; then cpp=$(compgen -c | grep "^cpp-"); fi;; esac
  {
    echo "let tracing = false"
    echo "let tracking = false"
    echo "let track_n = 2"
    echo "let experimental = false"
    echo "let cpp = \"$cpp\""
  } >> src/config.ml
fi

if [ "$VERSION" ]; then
  grep -q "goblint = \"$VERSION\"" src/version.ml 2> /dev/null ||
    (sed "s/goblint = .*/goblint = \"$VERSION\"/" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi

if [ "$CILVERSION" ]; then
  grep -q "cil = \"$CILVERSION\"" src/version.ml 2> /dev/null ||
    (sed "s/cil = .*/cil = \"$CILVERSION\"/" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi
