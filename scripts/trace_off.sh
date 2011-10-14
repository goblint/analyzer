#/bin/bash
grep -q 'tracing = true' src/version.ml && \
  sed -i 's/tracing = true/tracing = false/' src/version.ml
make
