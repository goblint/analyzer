#!/bin/bash
VERSION=`git describe --tags`
REL=goblint-$VERSION
mkdir $REL
cp goblint.exe $REL
cp LICENSE* $REL
git checkout-index -a --prefix=$REL/tmp/
mv $REL/tmp/includes $REL
mv $REL/tmp/tests $REL
rm -rf $REL/tmp
zip -qr $REL.zip $REL
rm -rf $REL
