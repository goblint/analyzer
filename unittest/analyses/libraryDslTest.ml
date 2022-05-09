open OUnit2

let memset_desc: LibraryDesc.desc = LibraryDsl.(
  [drop "dest" [w]; drop "ch" []; drop "count" []] >> `Unknown
)

let pthread_mutex_lock_desc: LibraryDesc.desc = LibraryDsl.(
  [__' [r]] >> fun e -> `Lock e
)

let pthread_create_desc: LibraryDesc.desc = LibraryDsl.(
  [__ "thread" [w]; drop "attr" [r]; __ "start_routine" [r]; __ "arg" [r]] >> fun thread start_routine arg -> `ThreadCreate (thread, start_routine, arg)
)

let realloc_desc: LibraryDesc.desc = LibraryDsl.(
  [__ "ptr" [r; f]; __ "size" []] >> fun ptr size -> `Realloc (ptr, size)
)

let scanf_desc': LibraryDesc.desc = LibraryDsl.(
  (drop "format" []) :: Var (__' [w]) >> fun (args: Cil.exp list) -> `Unknown
)

let scanf_desc: LibraryDesc.desc = LibraryDsl.(
  (drop "format" []) :: Var (drop' [w]) >> `Unknown
)

let rand_desc: LibraryDesc.desc = LibraryDsl.(
  unknown ~attrs:[`ThreadUnsafe] []
)

let tests =
  "libraryDslTest" >::: [

  ]
