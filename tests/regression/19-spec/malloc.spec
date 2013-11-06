w1 "pointer is not saved [leak]"
w2 "freeing unallocated pointer $ [segfault?]"
w3 "writing to unallocated pointer $ [segfault?]"
w4 "overwriting unfreed pointer $ [leak]"

1        -w1> 1        malloc(_)
1        -w2> 1        free($p)
1        -w3> 1        *$p = _
1        ->   u_alloc  $p = malloc($size) // TODO does compiler check size?

u_alloc  ->   1        branch($key==0, true)
u_alloc  ->   alloc    branch($key==0, false)

alloc    -w4> alloc    $p = malloc($size)
alloc    ->   freed    free($p)

freed    ->>  1        _ // let state 1 handle the rest

// setup which states are end states
1        ->   end      _
freed    ->   end      _
// warning for all entries that are not in an end state
_end "pointer is never freed"
_END "unfreed pointers: $"