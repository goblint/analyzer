#/bin/bash
grep -q 'tracking = true' src/config.ml && \
  sed -i"" 's/tracking = true/tracking = false/' src/config.ml
make
