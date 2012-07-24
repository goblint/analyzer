#/bin/bash
VERSION=`git describe --tags --dirty 2> /dev/null | sed s/^v//`
CILVERSION=`git --git-dir=../cil/.git describe --tags 2> /dev/null | sed s/^cil-//`

if [ ! -f src/version.ml ]; then
  echo "let goblint = \"unknown\"" > src/version.ml
  echo "let cil = \"unknown\"" >> src/version.ml
fi

if [ ! -f src/gobconfig.ml ]; then
  echo "let tracing = false" > src/gobconfig.ml
  echo "let tracking = false" >> src/gobconfig.ml
  echo "let track_n = 2" >> src/gobconfig.ml
  echo "let experimental = false" >> src/gobconfig.ml
fi

if [ $VERSION ]; then
  grep -q "goblint = \"$VERSION\"" src/version.ml 2> /dev/null ||
    (sed "s/goblint = .*/goblint = \"$VERSION\"/" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi

if [ $CILVERSION ]; then
  grep -q "cil = \"$CILVERSION\"" src/version.ml 2> /dev/null ||
    (sed "s/cil = .*/cil = \"$CILVERSION\"/" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi
