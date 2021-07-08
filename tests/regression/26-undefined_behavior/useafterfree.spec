w1 "behavior.undefined.use_after_free"

1 -> 2 $p = malloc(_)
2 -> 3 free($p)
3 -> 2 $p = malloc(_)
3 -w1> 3 *$p = _
1 -> end _
3 -> end _
