#!/bin/bash
REL=goblint-0.9.5-alpha1
mkdir $REL
svn export . $REL/goblint
svn export ../cil $REL/cil
cp goblint $REL/goblint
cp scripts/make_dist.sh-aux/* $REL
tar czf $REL.tgz $REL
rm -rf $REL
