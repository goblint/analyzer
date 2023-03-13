Run Goblint on initial program version

  $ goblint --conf 06-renamed_param_usage_changed.json --enable incremental.save 06-renamed_param_usage_changed.c > /dev/null 2>&1

Apply patch

  $ chmod +w 06-renamed_param_usage_changed.c
  $ patch -b <06-renamed_param_usage_changed.patch
  patching file 06-renamed_param_usage_changed.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 06-renamed_param_usage_changed.json --enable incremental.load 06-renamed_param_usage_changed.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 0; removed = 0

Revert patch

  $ patch -b -R <06-renamed_param_usage_changed.patch
  patching file 06-renamed_param_usage_changed.c
