#/bin/bash
grep -q 'tracking = false' src/config.ml && \
  sed 's/tracking = false/tracking = true/' src/config.ml > src/config.tmp && mv src/config.tmp src/config.ml
make
