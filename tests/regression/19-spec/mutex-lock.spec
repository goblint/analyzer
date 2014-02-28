w1 "unlocking not locked mutex"
w2 "locking already locked mutex"

1        -w1> 1        pthread_mutex_unlock($p)
1        ->   lock     pthread_mutex_lock($p)

lock     -w2> lock     pthread_mutex_lock($p)
lock     ->   1        pthread_mutex_unlock($p)

// setup which states are end states
1        -> end        _
// warning for all entries that are not in an end state
_end "mutex is never unlocked"
_END "locked mutexes: $"



//w1 "joining not created thread"
//w2 "overwriting id of already created thread"
//
//1        -w1> 1        pthread_join ($p, _)
//1        ->   created  pthread_create($p, _, _, _)
//
//created  -w2> created  pthread_create($p, _, _, _)
//created  ->   1        pthread_join ($p, _)
//
//// setup which states are end states
//1        -> end        _
//// warning for all entries that are not in an end state
//_end "thread is never joined"
//_END "unjoined threads: $"