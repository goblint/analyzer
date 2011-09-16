#/bin/bash
grep -q 'tracing = false' src/util/messages.ml && \
  sed -i 's/tracing = false/tracing = true/' src/util/messages.ml
make
