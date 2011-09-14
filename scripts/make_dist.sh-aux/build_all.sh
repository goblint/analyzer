#!/bin/bash
cd cil
./configure
make
cd ../analyzer
./configure
make
