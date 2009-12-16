#!/bin/bash
cd cil
./configure
make
cd ../goblint
./configure
make
