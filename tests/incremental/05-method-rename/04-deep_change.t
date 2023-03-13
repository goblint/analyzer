Run Goblint on initial program version

  $ goblint --conf 04-deep_change.json --enable incremental.save 04-deep_change.c > /dev/null 2>&1

Apply patch

  $ chmod +w 04-deep_change.c
  $ patch -b <04-deep_change.patch
  patching file 04-deep_change.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 04-deep_change.json --enable incremental.load 04-deep_change.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 0; removed = 0

Revert patch

  $ patch -b -R <04-deep_change.patch
  patching file 04-deep_change.c
