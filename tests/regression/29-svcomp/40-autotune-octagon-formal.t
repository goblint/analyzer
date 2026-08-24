Autotune octagon should add __goblint_relation_track__ attribute to formal parameters, not just locals.

Attribute must appear on the formal parameter in the function signature:
  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[*] octagon --enable justcil --set dbg.justcil-printer clean --set lib.activated '[]' 40-autotune-octagon-formal.c 2>&1 | grep -c "two.*__goblint_relation_track__"
  1

Attribute must appear on the local variable declaration:
  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[*] octagon --enable justcil --set dbg.justcil-printer clean --set lib.activated '[]' 40-autotune-octagon-formal.c 2>&1 | grep -c "one.*__goblint_relation_track__"
  1
