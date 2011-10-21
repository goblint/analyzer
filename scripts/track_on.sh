#/bin/bash
grep -q 'tracking = false' src/config.ml && \
  sed -i"" 's/tracking = false/tracking = true/' src/config.ml
make
