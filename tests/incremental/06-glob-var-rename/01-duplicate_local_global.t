Run Goblint on initial program version

  $ goblint --conf 01-duplicate_local_global.json --enable incremental.save 01-duplicate_local_global.c > /dev/null 2>&1

Apply patch

  $ chmod +w 01-duplicate_local_global.c
  $ patch -b <01-duplicate_local_global.patch
  patching file 01-duplicate_local_global.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 01-duplicate_local_global.json --enable incremental.load 01-duplicate_local_global.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 0 (with unchangedHeader = 0); added = 0; removed = 0

Revert patch

  $ patch -b -R <01-duplicate_local_global.patch
  patching file 01-duplicate_local_global.c
