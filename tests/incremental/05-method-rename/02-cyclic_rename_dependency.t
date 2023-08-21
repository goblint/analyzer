Run Goblint on initial program version

  $ goblint --conf 02-cyclic_rename_dependency.json --enable incremental.save 02-cyclic_rename_dependency.c > /dev/null 2>&1

Apply patch

  $ chmod +w 02-cyclic_rename_dependency.c
  $ patch -b <02-cyclic_rename_dependency.patch
  patching file 02-cyclic_rename_dependency.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 02-cyclic_rename_dependency.json --enable incremental.load 02-cyclic_rename_dependency.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 2; removed = 2
