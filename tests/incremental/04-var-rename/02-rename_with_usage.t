Run Goblint on initial program version

  $ goblint --conf 02-rename_with_usage.json --enable incremental.save 02-rename_with_usage.c > /dev/null 2>&1

Apply patch

  $ chmod +w 02-rename_with_usage.c
  $ patch -b <02-rename_with_usage.patch
  patching file 02-rename_with_usage.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 02-rename_with_usage.json --enable incremental.load 02-rename_with_usage.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 0 (with unchangedHeader = 0); added = 0; removed = 0
