#/bin/bash
make gen
scripts/set_version.sh
grep -q 'tracing = true' src/config.ml && \
  sed 's/tracing = true/tracing = false/' src/config.ml > src/config.tmp && mv src/config.tmp src/config.ml
make
