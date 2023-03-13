Run Goblint on initial program version

  $ goblint --conf 03-cyclic_with_swap.json --enable incremental.save 03-cyclic_with_swap.c > /dev/null 2>&1

Apply patch

  $ chmod +w 03-cyclic_with_swap.c
  $ patch -b <03-cyclic_with_swap.patch
  patching file 03-cyclic_with_swap.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 03-cyclic_with_swap.json --enable incremental.load 03-cyclic_with_swap.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 3; removed = 2

Revert patch

  $ patch -b -R <03-cyclic_with_swap.patch
  patching file 03-cyclic_with_swap.c
