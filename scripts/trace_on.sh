#/bin/bash
cd src/util
sed 's/tracing = false/tracing = true/' messages.ml > messages.out
mv messages.out messages.ml
cd ../..
make
