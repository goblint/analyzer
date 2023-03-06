#!/usr/bin/env bash

echo "You can also change profile to \"trace\" in ./dune-workspace directly!"

if grep -qs '(profile .*)' dune-workspace; then
  sed -i.bak 's/(profile .*)/(profile trace)/' dune-workspace && rm dune-workspace.bak
else
  echo "(lang dune 2.8)" >> dune-workspace
  echo "(profile trace)" >> dune-workspace
fi

make
