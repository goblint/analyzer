(** Library function descriptor (specification). *)

module Cil = GoblintCil
open Cil
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
  | Nan of (CilType.Fkind.t * Basetype.CilExp.t)
  | Inf of CilType.Fkind.t
  | Isfinite of Basetype.CilExp.t
  | Isinf of Basetype.CilExp.t
  | Isnan of Basetype.CilExp.t
  | Isnormal of Basetype.CilExp.t
  | Signbit of Basetype.CilExp.t
  | Isgreater of (Basetype.CilExp.t * Basetype.CilExp.t)
  | Isgreaterequal of (Basetype.CilExp.t * Basetype.CilExp.t)
  | Isless of (Basetype.CilExp.t * Basetype.CilExp.t)
  | Islessequal of (Basetype.CilExp.t * Basetype.CilExp.t)
  | Islessgreater of (Basetype.CilExp.t * Basetype.CilExp.t)
  | Isunordered of (Basetype.CilExp.t * Basetype.CilExp.t)
  | Abs of (CilType.Ikind.t * Basetype.CilExp.t)
  | Ceil of (CilType.Fkind.t * Basetype.CilExp.t)
  | Floor of (CilType.Fkind.t * Basetype.CilExp.t)
  | Fabs of (CilType.Fkind.t * Basetype.CilExp.t)
  | Fmax of (CilType.Fkind.t * Basetype.CilExp.t * Basetype.CilExp.t)
  | Fmin of (CilType.Fkind.t * Basetype.CilExp.t * Basetype.CilExp.t)
  | Acos of (CilType.Fkind.t * Basetype.CilExp.t)
  | Asin of (CilType.Fkind.t * Basetype.CilExp.t)
  | Atan of (CilType.Fkind.t * Basetype.CilExp.t)
  | Atan2 of (CilType.Fkind.t * Basetype.CilExp.t * Basetype.CilExp.t)
  | Cos of (CilType.Fkind.t * Basetype.CilExp.t)
  | Sin of (CilType.Fkind.t * Basetype.CilExp.t)
  | Tan of (CilType.Fkind.t * Basetype.CilExp.t)
  | Sqrt of (CilType.Fkind.t * Basetype.CilExp.t) [@@deriving eq, ord, hash]

(** Type of special function, or {!Unknown}. *)
(* Use inline record if not single {!Cil.exp} argument. *)
type special =
  | Alloca of Cil.exp
  | Malloc of Cil.exp
  | Calloc of { count: Cil.exp; size: Cil.exp; }
  | Realloc of { ptr: Cil.exp; size: Cil.exp; }
  | Free of Cil.exp
  | Assert of { exp: Cil.exp; check: bool; refine: bool; }
  | Lock of { lock: Cil.exp; try_: bool; write: bool; return_on_success: bool; }
  | Unlock of Cil.exp
  | ThreadCreate of { thread: Cil.exp; start_routine: Cil.exp; arg: Cil.exp; multiple: bool }
  | ThreadJoin of { thread: Cil.exp; ret_var: Cil.exp; }
  | ThreadExit of { ret_val: Cil.exp; }
  | ThreadSelf
  | Globalize of Cil.exp
  | Signal of Cil.exp
  | Broadcast of Cil.exp
  | MutexAttrSetType of { attr:Cil.exp; typ: Cil.exp; }
  | MutexInit of { mutex:Cil.exp; attr: Cil.exp; }
  (* All Sem specials  are not used yet. *)
  | SemInit of { sem: Cil.exp; pshared: Cil.exp; value: Cil.exp; }
  | SemWait of { sem: Cil.exp; try_:bool; timeout: Cil.exp option;}
  | SemPost of Cil.exp
  | SemDestroy of Cil.exp
  | Wait of { cond: Cil.exp; mutex: Cil.exp; }
  | TimedWait of { cond: Cil.exp; mutex: Cil.exp; abstime: Cil.exp; (** Unused *) }
  | Math of { fun_args: math; }
  | Memset of { dest: Cil.exp; ch: Cil.exp; count: Cil.exp; }
  | Bzero of { dest: Cil.exp; count: Cil.exp; }
  | Memcpy of { dest: Cil.exp; src: Cil.exp; n: Cil.exp; }
  | Strcpy of { dest: Cil.exp; src: Cil.exp; n: Cil.exp option; }
  | Strcat of { dest: Cil.exp; src: Cil.exp; n: Cil.exp option; }
  | Strlen of Cil.exp
  | Strstr of { haystack: Cil.exp; needle: Cil.exp; }
  | Strcmp of { s1: Cil.exp; s2: Cil.exp; n: Cil.exp option; }
  | Abort
  | Identity of Cil.exp (** Identity function. Some compiler optimization annotation functions map to this. *)
  | Setjmp of { env: Cil.exp; }
  | Longjmp of { env: Cil.exp; value: Cil.exp; }
  | Bounded of { exp: Cil.exp}  (** Used to check for bounds for termination analysis. *)
  | Rand
  | Unknown (** Anything not belonging to other types. *) (* TODO: rename to Other? *)


(** Pointer arguments access specification. *)
module Accesses =
struct
  type t = Cil.exp list -> (Access.t * Cil.exp list) list

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

module MathPrintable = struct
  include Printable.StdLeaf
  type t = math [@@deriving eq, ord, hash]

  let name () = "MathPrintable"

  let pretty () = function
    | Nan (fk, exp) -> Pretty.dprintf "(%a )nan(%a)" d_fkind fk d_exp exp
    | Inf fk -> Pretty.dprintf "(%a )inf()" d_fkind fk
    | Isfinite exp -> Pretty.dprintf "isFinite(%a)" d_exp exp
    | Isinf exp -> Pretty.dprintf "isInf(%a)" d_exp exp
    | Isnan exp -> Pretty.dprintf "isNan(%a)" d_exp exp
    | Isnormal exp -> Pretty.dprintf "isNormal(%a)" d_exp exp
    | Signbit exp -> Pretty.dprintf "signbit(%a)" d_exp exp
    | Isgreater (exp1, exp2) -> Pretty.dprintf "isGreater(%a, %a)" d_exp exp1 d_exp exp2
    | Isgreaterequal (exp1, exp2) -> Pretty.dprintf "isGreaterEqual(%a, %a)" d_exp exp1 d_exp exp2
    | Isless (exp1, exp2) -> Pretty.dprintf "isLess(%a, %a)" d_exp exp1 d_exp exp2
    | Islessequal (exp1, exp2) -> Pretty.dprintf "isLessEqual(%a, %a)" d_exp exp1 d_exp exp2
    | Islessgreater (exp1, exp2) -> Pretty.dprintf "isLessGreater(%a, %a)" d_exp exp1 d_exp exp2
    | Isunordered (exp1, exp2) -> Pretty.dprintf "isUnordered(%a, %a)" d_exp exp1 d_exp exp2
    | Abs (ik, exp) -> Pretty.dprintf "(%a )abs(%a)" d_ikind ik d_exp exp
    | Ceil (fk, exp) -> Pretty.dprintf "(%a )ceil(%a)" d_fkind fk d_exp exp
    | Floor (fk, exp) -> Pretty.dprintf "(%a )floor(%a)" d_fkind fk d_exp exp
    | Fabs (fk, exp) -> Pretty.dprintf "(%a )fabs(%a)" d_fkind fk d_exp exp
    | Fmax (fk, exp1, exp2) -> Pretty.dprintf "(%a )fmax(%a, %a)" d_fkind fk d_exp exp1 d_exp exp2
    | Fmin (fk, exp1, exp2) -> Pretty.dprintf "(%a )fmin(%a, %a)" d_fkind fk d_exp exp1 d_exp exp2
    | Acos (fk, exp) -> Pretty.dprintf "(%a )acos(%a)" d_fkind fk d_exp exp
    | Asin (fk, exp) -> Pretty.dprintf "(%a )asin(%a)" d_fkind fk d_exp exp
    | Atan (fk, exp) -> Pretty.dprintf "(%a )atan(%a)" d_fkind fk d_exp exp
    | Atan2 (fk, exp1, exp2) -> Pretty.dprintf "(%a )atan2(%a, %a)" d_fkind fk d_exp exp1 d_exp exp2
    | Cos (fk, exp) -> Pretty.dprintf "(%a )cos(%a)" d_fkind fk d_exp exp
    | Sin (fk, exp) -> Pretty.dprintf "(%a )sin(%a)" d_fkind fk d_exp exp
    | Tan (fk, exp) -> Pretty.dprintf "(%a )tan(%a)" d_fkind fk d_exp exp
    | Sqrt (fk, exp) -> Pretty.dprintf "(%a )sqrt(%a)" d_fkind fk d_exp exp

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module MathLifted = Lattice.FlatConf (struct
    include Printable.DefaultConf
    let top_name = "Unknown or no math desc"
    let bot_name = "Nonexistent math desc"
  end) (MathPrintable)
