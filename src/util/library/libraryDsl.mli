(** Library function descriptor DSL. *)

(** See {!LibraryFunctions} implementation for example usage. *)

open GoblintCil

(** Type parameters in this module can be ignored for usage.
    They are inferred automatically and used to ensure type-safety. *)

(** Argument descriptor. *)
type ('k, 'l, 'r) arg_desc

(** Arguments descriptor.
    Overrides standard list syntax. *)
type ('k, 'r) args_desc =
  | []: ('r, 'r) args_desc (** End of arguments. No more arguments may occur. *)
  | VarArgs: (_, 'l, 'r) arg_desc -> ('l, 'r) args_desc (** Variadic arguments, all with the same argument descriptor. Any number of arguments (including 0) may occur. *)
  | (::): ('k, _, 'm) arg_desc * ('m, 'r) args_desc -> ('k, 'r) args_desc (** Cons one argument descriptor. Argument must occur. *)


(** Create library function descriptor from arguments descriptor and continuation function, which takes as many arguments as are captured using {!__} and returns the corresponding {!LibraryDesc.type-special}. *)
val special: ?attrs:LibraryDesc.attr list -> ('k, LibraryDesc.special) args_desc -> 'k -> LibraryDesc.t

(** Create library function descriptor from arguments descriptor, which must {!drop} all arguments, and continuation function, which takes as an {!unit} argument and returns the corresponding {!LibraryDesc.type-special}.
    Unlike {!special}, this allows the {!LibraryDesc.type-special} of an argumentless function to depend on options, such that they aren't evaluated at initialization time in {!LibraryFunctions}.  *)
val special': ?attrs:LibraryDesc.attr list -> (LibraryDesc.special, LibraryDesc.special) args_desc -> (unit -> LibraryDesc.special) -> LibraryDesc.t

(** Create unknown library function descriptor from arguments descriptor, which must {!drop} all arguments. *)
val unknown: ?attrs:LibraryDesc.attr list -> (LibraryDesc.special, LibraryDesc.special) args_desc -> LibraryDesc.t

(** Argument access descriptor. *)
type access

(** Argument descriptor, which captures the named argument with accesses for continuation function of {!special}. *)
val __: string -> access list -> (Cil.exp -> 'r, Cil.exp list -> 'r, 'r) arg_desc

(** Argument descriptor, which captures an unnamed argument with accesses for continuation function of {!special}. *)
val __': access list -> (Cil.exp -> 'r, Cil.exp list -> 'r, 'r) arg_desc

(** Argument descriptor, which drops (does not capture) the named argument with accesses. *)
val drop: string -> access list -> ('r, 'r, 'r) arg_desc

(** Argument descriptor, which drops (does not capture) an unnamed argument with accesses. *)
val drop': access list -> ('r, 'r, 'r) arg_desc


(** Shallow {!AccessKind.Read} access.
    All immediate arguments of function calls are always read, this specifies the reading of pointed-to values. *)
val r: access

(** Deep {!AccessKind.Read} access.
    All immediate arguments of function calls are always read, this specifies the reading of pointed-to values.
    Rarely needed. *)
val r_deep: access

(** Shallow {!AccessKind.Write} access. *)
val w: access

(** Deep {!AccessKind.Write} access.
    Rarely needed. *)
val w_deep: access

(** Shallow {!AccessKind.Free} access. *)
val f: access

(** Deep {!AccessKind.Free} access.
    Rarely needed. *)
val f_deep: access

(** Shallow {!AccessKind.Spawn} access. *)
val s: access

(** Deep {!AccessKind.Spawn} access.
    Rarely needed. *)
val s_deep: access

(** Shallow {!AccessKind.Spawn} access, substituting function pointer calls for now (TODO). *)
val c: access

(** Deep {!AccessKind.Spawn} access, substituting deep function pointer calls for now (TODO)  *)
val c_deep: access

(** Conditional access, e.g. on an option. *)
val if_: (unit -> bool) -> access -> access
