w1 "pointer is not saved [leak]"
w2 "freeing unallocated pointer $ [segfault?]"
w3 "writing to unallocated pointer $ [segfault?]"
w4 "overwriting unfreed pointer $ [leak]"
w5 "freeing already freed pointer $ [double free!]"

1        -w1> 1        malloc(_)
1        -w2> 1        free($p)
1        -w3> 1        *$p = _
1        ->   alloc    $p = malloc(_) // TODO does compiler check size?

alloc    -w4> alloc    $p = malloc(_)
alloc    ->   freed    free($p)

freed    -w5> freed    free($p)
freed    ->> 1         _ // let state 1 handle the rest

// setup which states are end states
1        -> end        _
freed    -> end        _
// warning for all entries that are not in an end state
_end "pointer is never freed"
_END "unfreed pointers: $"