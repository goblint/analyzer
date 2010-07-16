#!/bin/bash
REL=goblint-0.9.5-alpha1
mkdir $REL
cp goblint $REL
cp LICENSE* $REL
svn export includes $REL/includes
svn export tests/regression $REL/tests
tar czf $REL.tgz $REL
rm -rf $REL
