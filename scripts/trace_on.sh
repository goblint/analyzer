#/bin/bash
grep -q 'tracing = false' src/config.ml && \
  sed -i"" 's/tracing = false/tracing = true/' src/config.ml
make
