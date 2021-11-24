#/usr/bin/env bash

echo "Just change dune-workspace directly!"

if grep -q '(profile .*)' dune-workspace; then
  sed -i 's/(profile .*)/(profile trace)/' dune-workspace
else
  echo "(lang dune 2.8)" >> dune-workspace
  echo "(profile trace)" >> dune-workspace
fi

make
