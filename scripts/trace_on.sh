#/bin/bash
grep -q 'tracing = false' src/config.ml && \
  sed 's/tracing = false/tracing = true/' src/config.ml > src/config.tmp && mv src/config.tmp src/config.ml
make
