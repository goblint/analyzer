#/bin/bash
grep -q 'tracing = true' src/config.ml && \
  sed -i"" 's/tracing = true/tracing = false/' src/config.ml
make
