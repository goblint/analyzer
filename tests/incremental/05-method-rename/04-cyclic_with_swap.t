Run Goblint on initial program version

  $ goblint --conf 04-cyclic_with_swap.json --enable incremental.save 04-cyclic_with_swap.c > /dev/null 2>&1

Apply patch

  $ chmod +w 04-cyclic_with_swap.c
  $ patch -b <04-cyclic_with_swap.patch
  patching file 04-cyclic_with_swap.c

Run Goblint incrementally on new program version and check the change detection result

  $ goblint --conf 04-cyclic_with_swap.json --enable incremental.load 04-cyclic_with_swap.c | grep 'change_info' | sed -r 's/^change_info = \{ unchanged = [[:digit:]]+; (.*) \}$/\1/'
  changed = 1 (with unchangedHeader = 1); added = 3; removed = 2

Revert patch

  $ patch -b -R <04-cyclic_with_swap.patch
  patching file 04-cyclic_with_swap.c
