  $ goblint --enable witness.yaml.enabled 81-recursive-local-address.c > /dev/null

The witness may name main's y through q. It must not equate p with the
current frame's x: only the caller's x has had its address passed to f.

  $ grep -E 'value: (p == & *x|q == & *y)$' witness.yml | sort -u
        value: q == & y
