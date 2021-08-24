#!/usr/bin/env bash
VERSION=$(git describe --all --long --dirty 2> /dev/null)
CILVERSION=$(git --git-dir=../cil/.git describe --tags 2> /dev/null | sed s@^cil-@@)

if [ ! -f src/version.ml ]; then
  {
    echo "let goblint = \"unknown\""
    echo "let cil = \"unknown\""
  } >> src/version.ml
fi

if [ ! -f src/config.ml ]; then
  {
    echo "let tracing = false"
    echo "let tracking = false"
    echo "let experimental = false"
    echo "let cpp = \"cpp\""
  } >> src/config.ml
fi

# if we are on OS X and cpp is the Apple LLVM version, we should look for some Homebrew cpp-8 (or other version)
case $OSTYPE in
  darwin*)
    if [[ $(compgen -c cpp-) ]]; then
      cpp=$(compgen -c cpp- | head -n1)
    else
      cpp="cpp"
      echo "Warning! GNU cpp not found! Goblint does not work with clang."
      echo "Please install gcc using homebrew."
    fi
    grep -q "cpp = \"$cpp\"" src/config.ml 2> /dev/null ||
      (sed "s@cpp = .*@cpp = \"$cpp\"@" src/config.ml > src/config.tmp && mv src/config.tmp src/config.ml &&
        echo "Set cpp command to \"$cpp\".")
    ;;
esac

if [ "$VERSION" ]; then
  grep -q "goblint = \"$VERSION\"" src/version.ml 2> /dev/null ||
    (sed "s@goblint = .*@goblint = \"$VERSION\"@" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi

if [ "$CILVERSION" ]; then
  grep -q "cil = \"$CILVERSION\"" src/version.ml 2> /dev/null ||
    (sed "s@cil = .*@cil = \"$CILVERSION\"@" src/version.ml > src/version.tmp && mv src/version.tmp src/version.ml)
fi
