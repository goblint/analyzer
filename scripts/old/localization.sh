#!/bin/bash
cd tests/localization
rm -rf result
for x in `find * -name "*.c" | sort`
do
  echo "Testing $x"
  ../../goblint --conf flags.json $x
done
