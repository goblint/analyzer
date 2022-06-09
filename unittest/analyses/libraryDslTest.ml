open OUnit2

let memset_desc: LibraryDesc.t = LibraryDsl.(
  unknown [drop "dest" [w]; drop "ch" []; drop "count" []]
)

let pthread_mutex_lock_desc: LibraryDesc.t = LibraryDsl.(
  special [__' [r]] @@ fun e -> Lock { lock = e; try_ = false; write = true; return_on_success = false; } (* actual description in LibraryFunctions has try_ depending on sem.lock.fail *)
)

let pthread_create_desc: LibraryDesc.t = LibraryDsl.(
  special [__ "thread" [w]; drop "attr" [r]; __ "start_routine" [r]; __ "arg" [r]] @@ fun thread start_routine arg -> ThreadCreate { thread; start_routine; arg }
)

let realloc_desc: LibraryDesc.t = LibraryDsl.(
  special [__ "ptr" [r; f]; __ "size" []] @@ fun ptr size -> Realloc { ptr; size }
)

let scanf_desc': LibraryDesc.t = LibraryDsl.(
  special ((drop "format" []) :: VarArgs (__' [w])) @@ fun (args: Cil.exp list) -> Unknown
)

let scanf_desc: LibraryDesc.t = LibraryDsl.(
  unknown ((drop "format" []) :: VarArgs (drop' [w]))
)

let rand_desc: LibraryDesc.t = LibraryDsl.(
  unknown ~attrs:[ThreadUnsafe] []
)

let tests =
  "libraryDslTest" >::: [

  ]
