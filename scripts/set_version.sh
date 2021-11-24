#!/usr/bin/env bash
VERSION=$(git describe --all --long --dirty 2> /dev/null)
CILVERSION=$(git --git-dir=../cil/.git describe --tags 2> /dev/null | sed s@^cil-@@)

if [ ! -f src/version.ml ]; then
  {
    echo "let goblint = \"unknown\""
    echo "let cil = \"unknown\""
  } >> src/version.ml
fi

if [ "$VERSION" ]; then
  grep -q "goblint = \"$VERSION\"" src/version.ml 2> /dev/null ||
    (sed "s@goblint = .*@goblint = \"$VERSION\"@" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi

if [ "$CILVERSION" ]; then
  grep -q "cil = \"$CILVERSION\"" src/version.ml 2> /dev/null ||
    (sed "s@cil = .*@cil = \"$CILVERSION\"@" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi
