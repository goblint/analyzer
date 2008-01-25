#/bin/bash
cd src/util
sed 's/tracking = true/tracking = false/' progress.ml > progress.out
mv progress.out progress.ml
cd ../..
make
