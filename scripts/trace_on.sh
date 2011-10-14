#/bin/bash
grep -q 'tracing = false' src/version.ml && \
  sed -i 's/tracing = false/tracing = true/' src/version.ml
make
