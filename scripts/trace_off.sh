#/bin/bash
cd src/util
sed 's/tracing = true/tracing = false/' messages.ml > messages.out
mv messages.out messages.ml
cd ../..
make
