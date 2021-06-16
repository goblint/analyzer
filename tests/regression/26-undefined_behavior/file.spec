w1 "pointer is not saved [leak]"
w2 "freeing unallocated pointer $ [segfault?]"
w3 "writing to unallocated pointer $ [segfault?]"
w4 "overwriting unfreed pointer $ [leak]"
w5 "use after free $ [segfault?]"

1        -w1> 1        malloc(_)
1        -w2> 1        free($p)
1        -w3> 1        *$p = _
1        ->   u_alloc  $p = malloc(_)
1        -w5> 1        free($p), *$p = _

u_alloc  ->   1        branch($p==0, true)
u_alloc  ->   alloc    branch($p==0, false)

alloc    -w4> alloc    $p = malloc(_)
alloc    ->   freed    free($p)
freed    -w5> freed    *$p=_

freed    ->>  1        _ // let state 1 handle the rest

// setup which states are end states
end: 1, freed
// warning for all entries that are not in an end state
!end "pointer is never freed"
!end@return "unfreed pointers: $"