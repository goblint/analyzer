type ('k, 'l, 'r) arg_desc

type ('k, 'r) args_desc =
  | []: ('r, 'r) args_desc
  | VarArgs: (_, 'l, 'r) arg_desc -> ('l, 'r) args_desc
  | (::): ('k, _, 'm) arg_desc * ('m, 'r) args_desc -> ('k, 'r) args_desc


val special: ?attrs:LibraryDesc.attr list -> ('k, LibraryDesc.special) args_desc -> 'k -> LibraryDesc.desc
val unknown: ?attrs:LibraryDesc.attr list -> ([> `Unknown ], LibraryDesc.special) args_desc -> LibraryDesc.desc


val __: string -> LibraryDesc.access list -> (Cil.exp -> 'r, Cil.exp list -> 'r, 'r) arg_desc
val __': LibraryDesc.access list -> (Cil.exp -> 'r, Cil.exp list -> 'r, 'r) arg_desc

val drop: string -> LibraryDesc.access list -> ('r, 'r, 'r) arg_desc
val drop': LibraryDesc.access list -> ('r, 'r, 'r) arg_desc


val r: [> `Read ]
val w: [> `Write ]
val f: [> `Free ]
