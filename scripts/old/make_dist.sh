#!/bin/bash
make
#VERSION=`git describe --tags`
REL=goblint
mkdir -p mkdist_tmp/$REL
git checkout-index -a --prefix=mkdist_tmp/$REL/analyzer/
git --git-dir=../cil/.git checkout-index -a --prefix=mkdist_tmp/$REL/cil/
#cp goblint $REL/analyzer
cp src/version.ml mkdist_tmp/$REL/analyzer/src
cp scripts/make_dist.sh-aux/* mkdist_tmp/$REL
cd mkdist_tmp
tar czf ../$REL.tgz $REL
cd ..
rm -rf mkdist_tmp
