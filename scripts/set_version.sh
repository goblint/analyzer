#/bin/bash
VERSION=`git describe --tags --dirty 2> /dev/null | sed s/^v//`
CILVERSION=`git --git-dir=../cil/.git describe --tags 2> /dev/null | sed s/^cil-//`

if [ ! -f src/version.ml ]; then
  echo "let goblint = \"unknown\"" > src/version.ml
  echo "let cil = \"unknown\"" >> src/version.ml
fi

if [ $VERSION ]; then
  grep -q "goblint = \"$VERSION\"" src/version.ml 2> /dev/null ||
    sed -i "s/goblint = .*/goblint = \"$VERSION\"/" src/version.ml
fi

if [ $CILVERSION ]; then
  grep -q "cil = \"$CILVERSION\"" src/version.ml 2> /dev/null ||
    sed -i "s/cil = .*/cil = \"$CILVERSION\"/" src/version.ml
fi
