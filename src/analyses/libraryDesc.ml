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
  | Nan of (Cil.fkind * Cil.exp)
  | Inf of Cil.fkind
  | Isfinite of Cil.exp
  | Isinf of Cil.exp
  | Isnan of Cil.exp
  | Isnormal of Cil.exp
  | Signbit of Cil.exp
  | Isgreater of (Cil.exp * Cil.exp)
  | Isgreaterequal of (Cil.exp * Cil.exp)
  | Isless of (Cil.exp * Cil.exp)
  | Islessequal of (Cil.exp * Cil.exp)
  | Islessgreater of (Cil.exp * Cil.exp)
  | Isunordered of (Cil.exp * Cil.exp)
  | Ceil of (Cil.fkind * Cil.exp)
  | Floor of (Cil.fkind * Cil.exp)
  | Fabs of (Cil.fkind * Cil.exp)
  | Fmax of (Cil.fkind * Cil.exp * Cil.exp)
  | Fmin of (Cil.fkind * Cil.exp * Cil.exp)
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
  | Assert of { exp: Cil.exp; check: bool; refine: bool; }
  | Lock of { lock: Cil.exp; try_: bool; write: bool; return_on_success: bool; }
  | Unlock of Cil.exp
  | ThreadCreate of { thread: Cil.exp; start_routine: Cil.exp; arg: Cil.exp; }
  | ThreadJoin of { thread: Cil.exp; ret_var: Cil.exp; }
  | ThreadExit of { ret_val: Cil.exp; }
  | Signal of Cil.exp
  | Broadcast of Cil.exp
  | Wait of { cond: Cil.exp; mutex: Cil.exp; }
  | TimedWait of { cond: Cil.exp; mutex: Cil.exp; abstime: Cil.exp; (** Unused *) }
  | Math of { fun_args: math; }
  | Memset of { dest: Cil.exp; ch: Cil.exp; count: Cil.exp; }
  | Bzero of { dest: Cil.exp; count: Cil.exp; }
  | Memcpy of { dest: Cil.exp; src: Cil.exp }
  | Strcpy of { dest: Cil.exp; src: Cil.exp } (* TODO: add count for strncpy when actually used *)
  | Abort
  | Identity of Cil.exp (** Identity function. Some compiler optimization annotation functions map to this. *)
  | Setjmp of { env: Cil.exp; }
  | Longjmp of { env: Cil.exp; value: Cil.exp; }
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
  | `Lock (try_, write, return_on_success) ->
    begin match args with
      | [lock] -> Lock { lock ; try_; write; return_on_success; }
      | [] -> failwith "lock has no arguments"
      | _ -> failwith "lock has multiple arguments"
    end
  | `Unlock ->
    begin match args with
      | [arg] -> Unlock arg
      | [] -> failwith "unlock has no arguments"
      | _ -> failwith "unlock has multiple arguments"
    end
  | `ThreadCreate (thread, start_routine, arg) -> ThreadCreate { thread; start_routine; arg; }
  | `ThreadJoin (thread, ret_var) -> ThreadJoin { thread; ret_var; }
  | `Unknown _ -> Unknown

let of_old ?(attrs: attr list=[]) (old_accesses: Accesses.old) (classify_name): t = {
  attrs;
  accs = Accesses.of_old old_accesses;
  special = special_of_old classify_name;
}

module MathPrintable = struct
  include Printable.Std
  type t = math

  let name () = "MathPrintable"

  let relift = function
  | Nan (fk, exp) -> Nan (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Inf fk -> Inf (CilType.Fkind.relift fk)
  | Isfinite exp -> Isfinite (CilType.Exp.relift exp)
  | Isinf exp -> Isinf (CilType.Exp.relift exp)
  | Isnan exp -> Isnan (CilType.Exp.relift exp)
  | Isnormal exp -> Isnormal (CilType.Exp.relift exp)
  | Signbit exp -> Signbit (CilType.Exp.relift exp)
  | Isgreater (exp1, exp2) -> Isgreater (CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Isgreaterequal (exp1, exp2) -> Isgreaterequal (CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Isless (exp1, exp2) -> Isless (CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Islessequal (exp1, exp2) -> Islessequal (CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Islessgreater (exp1, exp2) -> Islessgreater (CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Isunordered (exp1, exp2) -> Isunordered (CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Ceil (fk, exp) -> Ceil (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Floor (fk, exp) -> Floor (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Fabs (fk, exp) -> Fabs (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Fmax (fk, exp1, exp2) -> Fmax (CilType.Fkind.relift fk, CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Fmin (fk, exp1, exp2) -> Fmin (fk, CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Acos (fk, exp) -> Acos (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Asin (fk, exp) -> Asin (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Atan (fk, exp) -> Atan (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Atan2 (fk, exp1, exp2) -> Atan2 (CilType.Fkind.relift fk, CilType.Exp.relift exp1, CilType.Exp.relift exp2)
  | Cos (fk, exp) -> Cos (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Sin (fk, exp) -> Sin (CilType.Fkind.relift fk, CilType.Exp.relift exp)
  | Tan (fk, exp) -> Tan (CilType.Fkind.relift fk, CilType.Exp.relift exp)


  let order = function
    | Nan _ -> 1
    | Inf _ -> 2
    | Isfinite _ -> 3
    | Isinf _ -> 4
    | Isnan _ -> 5
    | Isnormal _ -> 6
    | Signbit _ -> 7
    | Isgreater _ -> 8
    | Isgreaterequal _ -> 9
    | Isless _ -> 10
    | Islessequal _ -> 11
    | Islessgreater _ -> 12
    | Isunordered _ -> 13
    | Ceil _ -> 14
    | Floor _ -> 15
    | Fabs _ -> 16
    | Fmax _ -> 17
    | Fmin _ -> 18
    | Acos _ -> 19
    | Asin _ -> 20
    | Atan _ -> 21
    | Atan2 _ -> 22
    | Cos _ -> 23
    | Sin _ -> 24
    | Tan _ -> 25

  let equal m1 m2 = (compare m1 m2) == 0
  let hash = order

  let cmp_fk_exp (fk1, e1) (fk2, e2) =
    let r = (CilType.Fkind.compare fk1 fk2) in
    if r <> 0 then
      r
    else
      CilType.Exp.compare e1 e2

  let cmp_exp_exp (e1, e1') (e2, e2') =
    let r = (CilType.Exp.compare e1 e2) in
    if r <> 0 then
      r
    else
      CilType.Exp.compare e1' e2'

  let cmp_fk_exp_exp (fk1, e1, e1') (fk2, e2, e2') =
    let r = (CilType.Fkind.compare fk1 fk2) in
    if r <> 0 then
      r
    else
      cmp_exp_exp (e1, e1') (e2, e2')

  let compare m1 m2 =
    let r = Stdlib.compare (order m1) (order m2) in
    if r <> 0 then
      r
    else
      match m1, m2 with
      | Nan fe1, Nan fe2 -> cmp_fk_exp fe1 fe2
      | Inf fk1, Inf fk2 -> CilType.Fkind.compare fk1 fk2
      | Isfinite e1, Isfinite e2 -> CilType.Exp.compare e1 e2
      | Isinf e1, Isinf e2 -> CilType.Exp.compare e1 e2
      | Isnan e1, Isnan e2 -> CilType.Exp.compare e1 e2
      | Isnormal e1, Isnormal e2 -> CilType.Exp.compare e1 e2
      | Signbit e1, Signbit e2 -> CilType.Exp.compare e1 e2
      | Isgreater ee1, Isgreater ee2 -> cmp_exp_exp ee1 ee2
      | Isgreaterequal ee1, Isgreaterequal ee2 -> cmp_exp_exp ee1 ee2
      | Isless ee1, Isless ee2 -> cmp_exp_exp ee1 ee2
      | Islessequal ee1, Islessequal ee2 -> cmp_exp_exp ee1 ee2
      | Islessgreater ee1, Islessgreater ee2 -> cmp_exp_exp ee1 ee2
      | Isunordered ee1, Isunordered ee2 -> cmp_exp_exp ee1 ee2
      | Ceil fe1, Ceil fe2 -> cmp_fk_exp fe1 fe2
      | Floor fe1, Floor fe2 -> cmp_fk_exp fe1 fe2
      | Fabs fe1, Fabs fe2 -> cmp_fk_exp fe1 fe2
      | Fmax fee1, Fmax fee2 -> cmp_fk_exp_exp fee1 fee2
      | Fmin fee1, Fmin fee2 -> cmp_fk_exp_exp fee1 fee2
      | Acos fe1, Acos fe2 -> cmp_fk_exp fe1 fe2
      | Asin fe1, Asin fe2 -> cmp_fk_exp fe1 fe2
      | Atan fe1, Atan fe2 -> cmp_fk_exp fe1 fe2
      | Atan2 fee1, Atan2 fee2 -> cmp_fk_exp_exp fee1 fee2
      | Cos fe1, Cos fe2 -> cmp_fk_exp fe1 fe2
      | Sin fe1, Sin fe2 -> cmp_fk_exp fe1 fe2
      | Tan fe1, Tan fe2 -> cmp_fk_exp fe1 fe2
      | _ -> failwith "impossible"

  let show = function
    | Nan _ -> "nan"
    | Inf _ -> "inf"
    | Isfinite _ -> "isFinite"
    | Isinf _ -> "isInf"
    | Isnan _ -> "isNan"
    | Isnormal _ -> "isNormal"
    | Signbit _ -> "signbit"
    | Isgreater _ -> "isGreater"
    | Isgreaterequal _ -> "isGreaterEqual"
    | Isless _ -> "isLess"
    | Islessequal _ -> "isLessEqual"
    | Islessgreater _ -> "isLessGreater"
    | Isunordered _ -> "isUnordered"
    | Ceil _ -> "ceil"
    | Floor _ -> "floor"
    | Fabs _ -> "fabs"
    | Fmax _ -> "fmax"
    | Fmin _ -> "fmin"
    | Acos _ -> "acos"
    | Asin _ -> "asin"
    | Atan _ -> "atan"
    | Atan2 _ -> "atan2"
    | Cos _ -> "cos"
    | Sin _ -> "sin"
    | Tan _ -> "tan"

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

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)
  let to_yojson _ = failwith "ToDo Implement in future"
end

module MathLifted = Lattice.Flat (MathPrintable) (struct
  let top_name = "Unknown math desc"
  let bot_name = "Nonexistent math desc"
end)
