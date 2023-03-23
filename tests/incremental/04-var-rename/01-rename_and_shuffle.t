Run Goblint on initial program version

  $ goblint --conf 01-rename_and_shuffle.json --enable incremental.save 01-rename_and_shuffle.c > /dev/null 2>&1

Apply patch

  $ chmod +w 01-rename_and_shuffle.c
  $ patch -b <01-rename_and_shuffle.patch
  patching file 01-rename_and_shuffle.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 01-rename_and_shuffle.json --enable incremental.load 01-rename_and_shuffle.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 0; removed = 0
