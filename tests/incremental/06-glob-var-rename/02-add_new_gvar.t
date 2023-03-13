Run Goblint on initial program version

  $ goblint --conf 02-add_new_gvar.json --enable incremental.save 02-add_new_gvar.c > /dev/null 2>&1

Apply patch

  $ chmod +w 02-add_new_gvar.c
  $ patch -b <02-add_new_gvar.patch
  patching file 02-add_new_gvar.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 02-add_new_gvar.json --enable incremental.load 02-add_new_gvar.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 1; removed = 0

Revert patch

  $ patch -b -R <02-add_new_gvar.patch
  patching file 02-add_new_gvar.c
