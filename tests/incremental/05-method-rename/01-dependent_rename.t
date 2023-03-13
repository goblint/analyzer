Run Goblint on initial program version

  $ goblint --conf 01-dependent_rename.json --enable incremental.save 01-dependent_rename.c > /dev/null 2>&1

Apply patch

  $ chmod +w 01-dependent_rename.c
  $ patch -b <01-dependent_rename.patch
  patching file 01-dependent_rename.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 01-dependent_rename.json --enable incremental.load 01-dependent_rename.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 2; removed = 2

Revert patch

  $ patch -b -R <01-dependent_rename.patch
  patching file 01-dependent_rename.c
