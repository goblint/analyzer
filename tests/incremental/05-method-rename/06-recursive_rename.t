Run Goblint on initial program version

  $ goblint --conf 06-recursive_rename.json --enable incremental.save 06-recursive_rename.c > /dev/null 2>&1

Apply patch

  $ chmod +w 06-recursive_rename.c
  $ patch -b <06-recursive_rename.patch
  patching file 06-recursive_rename.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 06-recursive_rename.json --enable incremental.load 06-recursive_rename.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 0 (with unchangedHeader = 0); added = 0; removed = 0

Revert patch

  $ patch -b -R <06-recursive_rename.patch
  patching file 06-recursive_rename.c
