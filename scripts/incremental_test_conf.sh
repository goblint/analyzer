#!/usr/bin/env bash
if [ ! -f scripts/incremental_test_conf.sh ]; then
  echo "Please run from root of analyzer repo."
  exit 1
fi

rm -r out/zstd

timeout --foreground 3h ./scripts/incremental.sh ../test-repos/zstd dev 7543085013db1a20a848d166e5931edc49e3cc2f big-benchmarks build_compdb_zstd.sh 5
timeout --foreground 3h ./scripts/incremental.sh ../test-repos/zstd dev 7543085013db1a20a848d166e5931edc49e3cc2f big-benchmarks1 build_compdb_zstd.sh 5
timeout --foreground 3h ./scripts/incremental.sh ../test-repos/zstd dev 7543085013db1a20a848d166e5931edc49e3cc2f big-benchmarks2 build_compdb_zstd.sh 5
timeout --foreground 3h ./scripts/incremental.sh ../test-repos/zstd dev 7543085013db1a20a848d166e5931edc49e3cc2f big-benchmarks3 build_compdb_zstd.sh 5
