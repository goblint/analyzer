type ('k, 'l, 'r) arg_desc

type ('k, 'r) args_desc =
  | []: ('r, 'r) args_desc
  | VarArgs: (_, 'l, 'r) arg_desc -> ('l, 'r) args_desc
  | (::): ('k, _, 'm) arg_desc * ('m, 'r) args_desc -> ('k, 'r) args_desc


val special: ?attrs:LibraryDesc.attr list -> ('k, LibraryDesc.special) args_desc -> 'k -> LibraryDesc.t
val unknown: ?attrs:LibraryDesc.attr list -> (LibraryDesc.special, LibraryDesc.special) args_desc -> LibraryDesc.t


val __: string -> LibraryDesc.Access.t list -> (Cil.exp -> 'r, Cil.exp list -> 'r, 'r) arg_desc
val __': LibraryDesc.Access.t list -> (Cil.exp -> 'r, Cil.exp list -> 'r, 'r) arg_desc

val drop: string -> LibraryDesc.Access.t list -> ('r, 'r, 'r) arg_desc
val drop': LibraryDesc.Access.t list -> ('r, 'r, 'r) arg_desc


val r: LibraryDesc.Access.t
val w: LibraryDesc.Access.t
val f: LibraryDesc.Access.t
