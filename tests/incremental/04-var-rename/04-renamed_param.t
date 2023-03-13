Run Goblint on initial program version

  $ goblint --conf 04-renamed_param.json --enable incremental.save 04-renamed_param.c > /dev/null 2>&1

Apply patch

  $ chmod +w 04-renamed_param.c
  $ patch -b <04-renamed_param.patch
  patching file 04-renamed_param.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 04-renamed_param.json --enable incremental.load 04-renamed_param.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 0 (with unchangedHeader = 0); added = 0; removed = 0

Revert patch

  $ patch -b -R <04-renamed_param.patch
  patching file 04-renamed_param.c
