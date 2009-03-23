#/bin/bash
cd src/util
sed 's/tracking = false/tracking = true/' progress.ml > progress.out
mv progress.out progress.ml
cd ../..
make
