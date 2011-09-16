#/bin/bash
grep -q 'tracing = true' src/util/messages.ml && \
  sed -i 's/tracing = true/tracing = false/' src/util/messages.ml
make
