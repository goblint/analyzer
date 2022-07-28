(** Library function descriptor (specification). *)

(** Pointer argument access specification. *)
module Access =
struct
  type t = {
    kind: AccessKind.t; (** Kind of access. *)
    deep: bool; (** Depth of access
                    - Shallow only accesses directly pointed values (may point to).
                    - Deep additionally follows all pointers in values (reachable). Rarely needed. *)
  }
end

type math =
  | Nan of (Cil.fkind * Cil.exp)
  | Inf of Cil.fkind
  | Isfinite of Cil.exp
  | Isinf of Cil.exp
  | Isnan of Cil.exp
  | Isnormal of Cil.exp
  | Signbit of Cil.exp
  | Fabs of (Cil.fkind * Cil.exp)
  | Acos of (Cil.fkind * Cil.exp)
  | Asin of (Cil.fkind * Cil.exp)
  | Atan of (Cil.fkind * Cil.exp)
  | Atan2 of (Cil.fkind * Cil.exp * Cil.exp)
  | Cos of (Cil.fkind * Cil.exp)
  | Sin of (Cil.fkind * Cil.exp)
  | Tan of (Cil.fkind * Cil.exp)

(** Type of special function, or {!Unknown}. *)
(* Use inline record if not single {!Cil.exp} argument. *)
type special =
  | Malloc of Cil.exp
  | Calloc of { count: Cil.exp; size: Cil.exp; }
  | Realloc of { ptr: Cil.exp; size: Cil.exp; }
  | Assert of Cil.exp
  | Lock of { lock: Cil.exp; try_: bool; write: bool; return_on_success: bool; }
  | Unlock of Cil.exp
  | ThreadCreate of { thread: Cil.exp; start_routine: Cil.exp; arg: Cil.exp; }
  | ThreadJoin of { thread: Cil.exp; ret_var: Cil.exp; }
  | ThreadExit of { ret_val: Cil.exp; }
  | Math of { fun_args: math; }
  | Memset of { dest: Cil.exp; ch: Cil.exp; count: Cil.exp; }
  | Bzero of { dest: Cil.exp; count: Cil.exp; }
  | Abort
  | Unknown (** Anything not belonging to other types. *) (* TODO: rename to Other? *)


(** Pointer arguments access specification. *)
module Accesses =
struct
  type t = Cil.exp list -> (Access.t * Cil.exp list) list

  (* TODO: remove after migration *)
  type old = AccessKind.t -> Cil.exp list -> Cil.exp list
  let of_old (f: old): t = fun args ->
    [
      ({ kind = Read; deep = true; }, f Read args);
      ({ kind = Write; deep = true; }, f Write args);
      ({ kind = Free; deep = true; }, f Free args);
      ({ kind = Spawn; deep = true; }, f Spawn args);
    ]

  (* TODO: remove/rename after migration? *)
  let find (accs: t): Access.t -> Cil.exp list -> Cil.exp list = fun acc args ->
    BatOption.(List.assoc_opt acc (accs args) |? [])

  let find_kind (accs: t): AccessKind.t -> Cil.exp list -> Cil.exp list = fun kind args ->
    let f a = find accs a args in
    f { kind; deep = true; } @ f { kind; deep = false; }

  let iter (accs: t) (f: Access.t -> Cil.exp -> unit) args: unit =
    accs args
    |> List.iter (fun (acc, exps) ->
        List.iter (fun exp -> f acc exp) exps
      )

  let fold (accs: t) (f: Access.t -> Cil.exp -> 'a -> 'a) args (a: 'a): 'a =
    accs args
    |> List.fold_left (fun a (acc, exps) ->
        List.fold_left (fun a exp -> f acc exp a) a exps
      ) a
end

(** Function attribute. *)
type attr =
  | ThreadUnsafe (** Function is not thread-safe to call, e.g. due to its own internal (global) state.
                     @see <https://man7.org/linux/man-pages/man7/pthreads.7.html> for list of thread-unsafe functions under POSIX.
                     @see <https://github.com/goblint/analyzer/issues/723> for Goblint issue about the (future) use of this attribute. *)
  | InvalidateGlobals (** Function invalidates all globals when called. *) (* TODO: AccessGlobals of Access.t list? *)

(** Library function descriptor. *)
type t = {
  special: Cil.exp list -> special; (** Conversion to {!type-special} using arguments. *)
  accs: Accesses.t; (** Pointer arguments access specification. *)
  attrs: attr list; (** Attributes of function. *)
}

let special_of_old classify_name = fun args ->
  match classify_name args with
  | `Malloc e -> Malloc e
  | `Calloc (count, size) -> Calloc { count; size; }
  | `Realloc (ptr, size) -> Realloc { ptr; size; }
  | `Assert e -> Assert e
  | `Lock (try_, write, return_on_success) -> Lock { lock = List.hd args; try_; write; return_on_success; }
  | `Unlock -> Unlock (List.hd args)
  | `ThreadCreate (thread, start_routine, arg) -> ThreadCreate { thread; start_routine; arg; }
  | `ThreadJoin (thread, ret_var) -> ThreadJoin { thread; ret_var; }
  | `Unknown _ -> Unknown

let of_old ?(attrs: attr list=[]) (old_accesses: Accesses.old) (classify_name): t = {
  attrs;
  accs = Accesses.of_old old_accesses;
  special = special_of_old classify_name;
}
