#!/bin/bash
VERSION=`git describe --tags`
REL=goblint-$VERSION
OLD=$PWD
mkdir $REL
git checkout-index -a --prefix=$REL/analyzer/
cd ../cil
git checkout-index -a --prefix=$OLD/$REL/cil/
cd $OLD
#cp goblint $REL/analyzer
cp scripts/make_dist.sh-aux/* $REL
tar czf $REL.tgz $REL
rm -rf $REL
