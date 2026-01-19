(** {!Apron} domains. *)

open Batteries
open GoblintCil
open Pretty
(* A binding to a selection of Apron-Domains *)
open GobApron
open RelationDomain
open SharedFunctions

module M = Messages

(** Resources for working with Apron:
    - OCaml API docs: https://antoinemine.github.io/Apron/doc/api/ocaml/index.html
    - C API docs (better function descriptions): https://antoinemine.github.io/Apron/doc/api/c/index.html
    - CEA 2007 slides (overview, mathematical descriptions): https://antoinemine.github.io/Apron/doc/papers/expose_CEA_2007.pdf
    - C API docs PDF (alternative mathematical descriptions): https://antoinemine.github.io/Apron/doc/api/c/apron.pdf
    - heterogeneous environments: https://link.springer.com/chapter/10.1007%2F978-3-030-17184-1_26 (Section 4.1) *)

let widening_thresholds_apron = ResettableLazy.from_fun (fun () ->
    let t = if GobConfig.get_string "ana.apron.threshold_widening_constants" = "comparisons" then WideningThresholds.octagon_thresholds else WideningThresholds.thresholds_incl_mul2 in
    let r = List.map Scalar.of_z (WideningThresholds.Thresholds.elements (ResettableLazy.force t)) in
    Array.of_list r
  )

let reset_lazy () =
  ResettableLazy.reset widening_thresholds_apron

module V = RelationDomain.V


module type Manager =
sig
  type mt
  type t = mt Apron.Manager.t
  val mgr : mt Apron.Manager.t
  val name : unit -> string
end

(** Manager for the Oct domain, i.e. an octagon domain.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Oct.html *)
module OctagonManager =
struct
  type mt = Oct.t

  (* Type of the manager *)
  type t = mt Manager.t

  (* Create the manager *)
  let mgr =  Oct.manager_alloc ()
  let name () = "Octagon"
end

(** Manager for the Polka domain, i.e. a polyhedra domain.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Polka.html *)
module PolyhedraManager =
struct
  (** We chose a the loose polyhedra here, i.e. with polyhedra with no strict inequalities *)
  type mt = Polka.loose Polka.t
  type t = mt Manager.t
  (* Create manager that fits to loose polyhedra *)
  let mgr = Polka.manager_alloc_loose ()
  let name () = "Polyhedra"
end

(** Another manager for the Polka domain but specifically for affine equalities.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Polka.html *)
module AffEqManager =
struct
  (** Affine equalities in apron used for comparison with our own implementation *)
  type mt = Polka.equalities Polka.t
  type t = mt Manager.t
  let mgr = Polka.manager_alloc_equalities ()
  let name () = "ApronAffEq"
end

(** Manager for the Box domain, i.e. an interval domain.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Box.html*)
module IntervalManager =
struct
  type mt = Box.t
  type t = mt Manager.t
  let mgr = Box.manager_alloc ()
  let name () = "Interval"
end

let manager =
  lazy (
    let options =
      ["octagon", (module OctagonManager: Manager);
       "interval", (module IntervalManager: Manager);
       "polyhedra", (module PolyhedraManager: Manager);
       "affeq", (module AffEqManager: Manager)]
    in
    let domain = (GobConfig.get_string "ana.apron.domain") in
    match List.assoc_opt domain options with
    | Some man -> man
    | None -> failwith @@ "Apron domain " ^ domain ^ " is not supported. Please check the ana.apron.domain setting."
  )

let get_manager (): (module Manager) =
  Lazy.force manager

(* Generic operations on abstract values at level 1 of interface, there is also Abstract0 *)
module A = Abstract1

module Bounds (Man: Manager) =
struct
  type t = Man.mt A.t

  let bound_texpr d texpr1 =
    let bounds = A.bound_texpr Man.mgr d texpr1 in
    let min = SharedFunctions.int_of_scalar ~round:`Ceil bounds.inf in
    let max = SharedFunctions.int_of_scalar ~round:`Floor bounds.sup in
    (min, max)
end

(** Pure environment and transfer functions. *)
module type AOpsPure =
sig
  type t
  val add_vars : t -> Var.t list -> t
  val remove_vars : t -> Var.t list -> t
  val remove_filter : t -> (Var.t -> bool) -> t
  val keep_vars : t -> Var.t list -> t
  val keep_filter : t -> (Var.t -> bool) -> t
  val forget_vars : t -> Var.t list -> t
  val assign_exp : Queries.ask -> t -> Var.t -> exp -> bool Lazy.t -> t
  val assign_var : t -> Var.t -> Var.t -> t
  val substitute_exp : Queries.ask-> t -> Var.t -> exp -> bool Lazy.t -> t
end

(** Imperative in-place environment and transfer functions. *)
module type AOpsImperative =
sig
  type t
  val add_vars_with : t -> Var.t list -> unit
  val remove_vars_with : t -> Var.t list -> unit
  val remove_filter_with : t -> (Var.t -> bool) -> unit
  val keep_vars_with : t -> Var.t list -> unit
  val keep_filter_with : t -> (Var.t -> bool) -> unit
  val forget_vars_with : t -> Var.t list -> unit
  val assign_exp_with : Queries.ask -> t -> Var.t -> exp -> bool Lazy.t -> unit
  val assign_exp_parallel_with : Queries.ask -> t -> (Var.t * exp) list -> bool -> unit (* TODO: why this one isn't lazy? *)
  val assign_var_with : t -> Var.t -> Var.t -> unit
  val assign_var_parallel_with : t -> (Var.t * Var.t) list -> unit
  val substitute_exp_with : Queries.ask -> t -> Var.t -> exp -> bool Lazy.t-> unit
  val substitute_exp_parallel_with :
    Queries.ask -> t -> (Var.t * exp) list -> bool Lazy.t -> unit
  val substitute_var_with : t -> Var.t -> Var.t -> unit
end

module type AOpsImperativeCopy =
sig
  include AOpsImperative
  val copy : t -> t
end

(** Default implementations of pure functions from [copy] and imperative functions. *)
module AOpsPureOfImperative (AOpsImperative: AOpsImperativeCopy): AOpsPure with type t = AOpsImperative.t =
struct
  open AOpsImperative
  type nonrec t = t

  let add_vars d vs =
    let nd = copy d in
    add_vars_with nd vs;
    nd
  let remove_vars d vs =
    let nd = copy d in
    remove_vars_with nd vs;
    nd
  let remove_filter d f =
    let nd = copy d in
    remove_filter_with nd f;
    nd
  let keep_vars d vs =
    let nd = copy d in
    keep_vars_with nd vs;
    nd
  let keep_filter d f =
    let nd = copy d in
    keep_filter_with nd f;
    nd
  let forget_vars d vs =
    let nd = copy d in
    forget_vars_with nd vs;
    nd
  let assign_exp ask d v e no_ov =
    let nd = copy d in
    assign_exp_with ask nd v e no_ov;
    nd
  let assign_var d v v' =
    let nd = copy d in
    assign_var_with nd v v';
    nd
  let substitute_exp ask d v e no_ov =
    let nd = copy d in
    substitute_exp_with ask nd v e no_ov;
    nd
end

(** Extra functions that don't have the pure-imperative correspondence. *)
module type AOpsExtra =
sig
  type t
  val copy : t -> t
  val vars : t -> Var.t list
  type marshal
  val unmarshal : marshal -> t
  val marshal : t -> marshal
  val mem_var : t -> Var.t -> bool
  val assign_var_parallel' :
    t -> Var.t list -> Var.t list -> t
  val meet_tcons : Queries.ask -> t -> Tcons1.t -> exp -> t
  val to_lincons_array : t -> Lincons1.earray
  val of_lincons_array : Lincons1.earray -> t

  val cil_exp_of_lincons1: Lincons1.t -> exp option
  val invariant: t -> Lincons1.t list
end

module type AOps =
sig
  include AOpsExtra
  include AOpsImperative with type t := t
  include AOpsPure with type t := t
end

(** Convenience operations on A. *)
module AOps0 (Tracked: Tracked) (Man: Manager) =
struct
  open SharedFunctions
  module Bounds = Bounds (Man)
  module Arg = struct
    let allow_global = false
  end
  module Convert = Convert (V) (Bounds) (Arg) (Tracked)



  type t = Man.mt A.t

  type var = Var.t

  let env t = A.env t

  let copy = A.copy Man.mgr

  (* marshal type: Abstract0.t and an array of var names *)
  type marshal = Man.mt Abstract0.t * string array

  let unmarshal ((abstract0, vs): marshal): t =
    let vars = Array.map Var.of_string vs in
    (* We do not have real-valued vars, so we pass an empty array in their place. *)
    let env = Environment.make vars [||] in
    {abstract0; env}

  let vars x = Environment.ivars_only @@ A.env x

  let marshal (x: t): marshal =
    let vars = Array.map Var.to_string (Array.of_list (vars x)) in
    x.abstract0, vars

  let mem_var d v = Environment.mem_var (A.env d) v

  let envop f nd a =
    let env' = f (A.env nd) a in
    A.change_environment_with Man.mgr nd env' false

  let add_vars_with = envop Environment.add_vars
  let remove_vars_with = envop Environment.remove_vars
  let remove_filter_with = envop Environment.remove_filter
  let keep_vars_with = envop Environment.keep_vars
  let keep_filter_with = envop Environment.keep_filter


  let forget_vars_with nd vs =
    (* Unlike keep_vars_with, this doesn't check mem_var, but assumes valid vars, like assigns *)
    let vs' = Array.of_list vs in
    A.forget_array_with Man.mgr nd vs' false

  let assign_exp_with ask nd v e no_ov =
    match Convert.texpr1_of_cil_exp ask nd (A.env nd) e no_ov with
    | texpr1 ->
      if M.tracing then M.trace "apron" "assign_exp converted: %a" Texpr1.pretty texpr1;
      A.assign_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp _ ->
      if M.tracing then M.trace "apron" "assign_exp unsupported";
      forget_vars_with nd [v]

  let assign_exp_parallel_with ask nd ves no_ov =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition assigns with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.to_seq
      |> Seq.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp ask nd env e (Lazy.from_val no_ov) with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp _ -> None
        ))
      |> Seq.memoize
      |> Seq.partition (fun (_, e_opt) -> Option.is_some e_opt)
    in
    (* parallel assign supported *)
    let (supported_vs, texpr1s) =
      supported
      |> Seq.map (Tuple2.map2 Option.get)
      |> Seq.unzip
      |> Tuple2.map Array.of_seq Array.of_seq
    in
    A.assign_texpr_array_with Man.mgr nd supported_vs texpr1s None;
    (* forget unsupported *)
    let unsupported_vs =
      unsupported
      |> Seq.map fst
      |> Array.of_seq
    in
    A.forget_array_with Man.mgr nd unsupported_vs false

  let assign_var_with nd v v' =
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.assign_texpr_with Man.mgr nd v texpr1 None

  let assign_var_parallel_with nd vv's =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    let (vs, texpr1s) =
      vv's
      |> List.to_seq
      |> Seq.map (Tuple2.map2 (Texpr1.var env))
      |> Seq.unzip
      |> Tuple2.map Array.of_seq Array.of_seq
    in
    A.assign_texpr_array_with Man.mgr nd vs texpr1s None

  let assign_var_parallel' d vs v's = (* unpaired parallel assigns *)
    (* TODO: _with version? *)
    let env = A.env d in
    let vs = Array.of_list vs in
    let texpr1s =
      v's
      |> List.to_seq
      |> Seq.map (Texpr1.var env)
      |> Array.of_seq
    in
    A.assign_texpr_array Man.mgr d vs texpr1s None

  let substitute_exp_with ask nd v e no_ov =
    match Convert.texpr1_of_cil_exp ask nd (A.env nd) e no_ov with
    | texpr1 ->
      A.substitute_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp _ ->
      forget_vars_with nd [v]

  let substitute_exp_parallel_with ask nd ves no_ov =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition substitutes with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.to_seq
      |> Seq.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp ask nd env e no_ov with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp _ -> None
        ))
      |> Seq.memoize
      |> Seq.partition (fun (_, e_opt) -> Option.is_some e_opt)
    in
    (* parallel substitute supported *)
    let (supported_vs, texpr1s) =
      supported
      |> Seq.map (Tuple2.map2 Option.get)
      |> Seq.unzip
      |> Tuple2.map Array.of_seq Array.of_seq
    in
    A.substitute_texpr_array_with Man.mgr nd supported_vs texpr1s None;
    (* forget unsupported *)
    let unsupported_vs =
      unsupported
      |> Seq.map fst
      |> Array.of_seq
    in
    A.forget_array_with Man.mgr nd unsupported_vs false

  let substitute_var_with nd v v' =
    (* TODO: non-_with version? *)
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.substitute_texpr_with Man.mgr nd v texpr1 None

  let meet_tcons _ d tcons1 e =
    let earray = Tcons1.array_make (A.env d) 1 in
    Tcons1.array_set earray 0 tcons1;
    A.meet_tcons_array Man.mgr d earray

  let to_lincons_array d =
    A.to_lincons_array Man.mgr d

  let of_lincons_array (a: Apron.Lincons1.earray) =
    A.of_lincons_array Man.mgr a.array_env a
  let unify (a:t) (b:t) = A.unify Man.mgr a b

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1
end

module AOps (Tracked: Tracked) (Man: Manager) =
struct
  module AO0 = AOps0 (Tracked) (Man)
  include AO0
  include AOpsPureOfImperative (AO0)
end

module type SPrintable =
sig
  include Printable.S
  (* Functions for bot and top for particular environment. *)
  val top_env: Environment.t -> t
  val bot_env: Environment.t -> t
  val is_top_env: t -> bool
  val is_bot_env: t -> bool

  val unify: t -> t -> t
  val invariant: t -> Lincons1.t list
  val pretty_diff: unit -> t * t -> Pretty.doc
end

module DBase (Man: Manager): SPrintable with type t = Man.mt A.t =
struct
  include Printable.StdLeaf

  type t = Man.mt A.t

  let name () = "Apron " ^ Man.name ()

  (* Functions for bot and top for particular environment. *)
  let top_env = A.top    Man.mgr
  let bot_env = A.bottom Man.mgr
  let is_top_env = A.is_top Man.mgr
  let is_bot_env = A.is_bottom Man.mgr

  let invariant _ = []

  let show (x:t) =
    GobFormat.asprintf "%a (env: %a)" A.print x Environment.pp (A.env x)
  let pretty () (x:t) = text (show x)

  let equal x y =
    Environment.equal (A.env x) (A.env y) && A.is_eq Man.mgr x y

  let hash (x:t) =
    A.hash Man.mgr x

  let compare (x: t) (y: t): int =
    failwith "Apron.Abstract1 doesn't have total order" (* https://github.com/antoinemine/apron/issues/99 *)

  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nconstraints\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%a</value>\n</map>\n</value>\n" (XmlUtil.escape (GobFormat.asprint A.print x)) Environment.printXml (A.env x)

  let to_yojson (x: t) =
    let constraints =
      A.to_lincons_array Man.mgr x
      |> Lincons1Set.of_earray
      |> Lincons1Set.elements
      |> List.map (fun lincons1 -> `String (Lincons1.show lincons1))
    in
    `Assoc [
      ("constraints", `List constraints);
      ("env", Environment.to_yojson (A.env x));
    ]

  let unify x y =
    A.unify Man.mgr x y

  let pretty_diff () (x, y) =
    let lcx = A.to_lincons_array Man.mgr x in
    let lcy = A.to_lincons_array Man.mgr y in
    let diff = Lincons1Set.(diff (of_earray lcy) (of_earray lcx)) in
    Pretty.docList ~sep:(Pretty.text ", ") (fun lc -> Pretty.text (Lincons1.show lc)) () (Lincons1Set.elements diff)
end


module type SLattice =
sig
  include SPrintable
  include Lattice.S with type t := t
  val invariant: t -> Lincons1.t list
end

module DWithOps (Man: Manager) (D: SLattice with type t = Man.mt A.t) =
struct
  include D
  include AOps (Tracked) (Man)
  include Tracked

  let eval_interval ask = Bounds.bound_texpr

  (** Assert a constraint expression.

      LAnd, LOr, LNot are directly supported by Apron domain in order to
      confirm logic-containing Apron invariants from witness while deep-query is disabled *)
  let rec assert_constraint ask d e negate (no_ov: bool Lazy.t) =
    if M.tracing then M.trace "assert_constraint_apron" "%a ;;; %a" d_exp e d_plainexp e;
    match e with
    (* Apron doesn't properly meet with DISEQ constraints: https://github.com/antoinemine/apron/issues/37.
       Join Gt and Lt versions instead. *)
    | BinOp (Ne, lhs, rhs, intType) when not negate ->
      let assert_gt = assert_constraint ask d (BinOp (Gt, lhs, rhs, intType)) negate no_ov in
      let assert_lt = assert_constraint ask d (BinOp (Lt, lhs, rhs, intType)) negate no_ov in
      join assert_gt assert_lt
    | BinOp (Eq, lhs, rhs, intType) when negate ->
      let assert_gt = assert_constraint ask d (BinOp (Gt, lhs, rhs, intType)) (not negate) no_ov in
      let assert_lt = assert_constraint ask d (BinOp (Lt, lhs, rhs, intType)) (not negate) no_ov in
      join assert_gt assert_lt
    | BinOp (LAnd, lhs, rhs, intType) when not negate ->
      let assert_l = assert_constraint ask d lhs negate no_ov in
      let assert_r = assert_constraint ask d rhs negate no_ov in
      meet assert_l assert_r
    | BinOp (LAnd, lhs, rhs, intType) when negate ->
      let assert_l = assert_constraint ask d lhs negate no_ov in
      let assert_r = assert_constraint ask d rhs negate no_ov in
      join assert_l assert_r (* de Morgan *)
    | BinOp (LOr, lhs, rhs, intType) when not negate ->
      let assert_l = assert_constraint ask d lhs negate no_ov in
      let assert_r = assert_constraint ask d rhs negate no_ov in
      join assert_l assert_r
    | BinOp (LOr, lhs, rhs, intType) when negate ->
      let assert_l = assert_constraint ask d lhs negate no_ov in
      let assert_r = assert_constraint ask d rhs negate no_ov in
      meet assert_l assert_r (* de Morgan *)
    | UnOp (LNot,e,_) -> assert_constraint ask d e (not negate) no_ov
    | _ ->
      begin match Convert.tcons1_of_cil_exp ask d (A.env d) e negate no_ov with
        | tcons1 ->
          if M.tracing then M.trace "apron" "assert_constraint %a %a" d_exp e Tcons1.pretty tcons1;
          if M.tracing then M.trace "apron" "assert_constraint st: %a" D.pretty d;
          if M.tracing then M.trace "apron" "assert_constraint tcons1: %a" Tcons1.pretty tcons1;
          let r = meet_tcons ask d tcons1 e in
          if M.tracing then M.trace "apron" "assert_constraint r: %a" D.pretty r;
          r
        | exception Convert.Unsupported_CilExp reason ->
          if M.tracing then M.trace "apron" "assert_constraint %a unsupported: %s" d_exp e (SharedFunctions.show_unsupported_cilExp reason);
          d
      end

  (** Keep only box-representable constraints.
      Used for [diff-box] in {!invariant}. *)
  let boxify d =
    let {box1_env; interval_array}: A.box1 = A.to_box Man.mgr d in
    let ivs, fvs = Environment.vars box1_env in
    assert (Array.length fvs = 0); (* shouldn't ever contain floats *)
    A.of_box Man.mgr box1_env ivs interval_array

  let to_lincons_set d =
    Lincons1Set.of_earray (A.to_lincons_array Man.mgr d)

  let invariant d =
    (* Would like to minimize to get rid of multi-var constraints directly derived from one-var constraints,
       but not implemented in Apron at all: https://github.com/antoinemine/apron/issues/44 *)
    (* let d = A.copy Man.mgr d in
       A.minimize Man.mgr d; *)
    let lcd = to_lincons_set d in
    if GobConfig.get_bool "ana.apron.invariant.diff-box" then (
      (* diff via lincons *)
      (* TODO: is there benefit to also Lincons1Set.simplify before diff? might make a difference if y=0 is represented as y>=0 && y<=0 or not *)
      let b = boxify d in (* convert back to same Apron domain (instead of box) to make lincons use the same format (e.g. oct doesn't return equalities, but box does) *)
      let lcb = to_lincons_set b in
      Lincons1Set.diff lcd lcb
    )
    else
      lcd

  let invariant d =
    invariant d
    |> (if Oct.manager_is_oct Man.mgr then Lincons1Set.simplify else Fun.id)
    |> Lincons1Set.elements (* TODO: remove list conversion? *)
end

(** With heterogeneous environments. *)
module DHetero (Man: Manager): SLattice with type t = Man.mt A.t =
struct
  include DBase (Man)



  let gce (x: Environment.t) (y: Environment.t): Environment.t =
    let (xi, xf) = Environment.vars x in
    (* TODO: check type compatibility *)
    let i = Array.filter (Environment.mem_var y) xi in
    let f = Array.filter (Environment.mem_var y) xf in
    Environment.make i f

  let join x y =
    let x_env = A.env x in
    let y_env = A.env y in
    let c_env = gce x_env y_env in
    let x_c = A.change_environment Man.mgr x c_env false in
    let y_c = A.change_environment Man.mgr y c_env false in
    let join_c = A.join Man.mgr x_c y_c in
    let j_env = Environment.lce x_env y_env in
    A.change_environment Man.mgr join_c j_env false

  (* TODO: move to AOps *)
  let meet_lincons d lincons1 =
    let earray = Lincons1.array_make (A.env d) 1 in
    Lincons1.array_set earray 0 lincons1;
    A.meet_lincons_array Man.mgr d earray

  let strengthening j x y =
    (* TODO: optimize strengthening *)
    if M.tracing then M.traceli "apron" "strengthening %a" pretty j;
    let x_env = A.env x in
    let y_env = A.env y in
    let j_env = A.env j in
    let x_j = A.change_environment Man.mgr x j_env false in
    let y_j = A.change_environment Man.mgr y j_env false in
    let x_cons = A.to_lincons_array Man.mgr x_j in
    let y_cons = A.to_lincons_array Man.mgr y_j in
    let try_add_con j con1 =
      if M.tracing then M.tracei "apron" "try_add_con %a" Lincons1.pretty con1;
      let t = meet_lincons j con1 in
      let t_x = A.change_environment Man.mgr t x_env false in
      let t_y = A.change_environment Man.mgr t y_env false in
      let leq_x = A.is_leq Man.mgr x t_x in
      let leq_y = A.is_leq Man.mgr y t_y in
      if M.tracing then M.trace "apron" "t: %a" pretty t;
      if M.tracing then M.trace "apron" "t_x (leq x %B): %a" leq_x pretty t_x;
      if M.tracing then M.trace "apron" "t_y (leq y %B): %a" leq_y pretty t_y;
      if leq_x && leq_y then (
        if M.tracing then M.traceu "apron" "added";
        t
      )
      else (
        if M.tracing then M.traceu "apron" "not added";
        j
      )
    in
    let lincons1_array_of_earray (earray: Lincons1.earray) =
      Array.init (Lincons1.array_length earray) (Lincons1.array_get earray)
    in
    let x_cons1 = lincons1_array_of_earray x_cons in
    let y_cons1 = lincons1_array_of_earray y_cons in
    let cons1 =
      (* Whether [con1] contains a var in [env]. *)
      let env_exists_mem_con1 env con1 =
        try
          Lincons1.iter (fun coeff var ->
              (* Lincons1 from polyhedra may contain variable with zero coefficient.
                 These are silently not printed! *)
              if not (Coeff.is_zero coeff) && Environment.mem_var env var then
                raise Stdlib.Exit (* found *)
            ) con1;
          false
        with Stdlib.Exit -> (* found *)
          true
      in
      let env_exists_mem_con1 env con1 =
        let r = env_exists_mem_con1 env con1 in
        if M.tracing then M.trace "apron" "env_exists_mem_con1 %a %a -> %B" Environment.pretty env Lincons1.pretty con1 r;
        r
      in
      (* Heuristically reorder constraints to pass 36/12 with singlethreaded->multithreaded mode switching. *)
      (* Put those constraints which strictly are in one argument's env first, to (hopefully) ensure they remain. *)
      let (x_cons1_some_y, x_cons1_only_x) = Array.partition (env_exists_mem_con1 y_env) x_cons1 in
      let (y_cons1_some_x, y_cons1_only_y) = Array.partition (env_exists_mem_con1 x_env) y_cons1 in
      Array.concat [x_cons1_only_x; y_cons1_only_y; x_cons1_some_y; y_cons1_some_x]
    in
    let j = Array.fold_left try_add_con j cons1 in
    if M.tracing then M.traceu "apron" "-> %a" pretty j;
    j

  let empty_env = Environment.make [||] [||]

  (* top and bottom over the empty environment are different, pending  https://github.com/goblint/analyzer/issues/1380 *)
  let bot () =
    bot_env empty_env

  let top () =
    top_env empty_env

  let is_bot x = equal (bot ()) x
  let is_top x = equal (top ()) x

  let strengthening_enabled = GobConfig.get_bool "ana.apron.strengthening"

  let join x y =
    (* just to optimize joining folds, which start with bot *)
    if is_bot x then (* TODO: also for non-empty env *)
      y
    else if is_bot y then (* TODO: also for non-empty env *)
      x
    else (
      if M.tracing then M.traceli "apron" "join %a %a" pretty x pretty y;
      let j = join x y in
      if M.tracing then M.trace "apron" "j = %a" pretty j;
      let j =
        if strengthening_enabled then (* TODO: skip if same envs? *)
          strengthening j x y
        else
          j
      in
      if M.tracing then M.traceu "apron" "-> %a" pretty j;
      j
    )

  let meet x y =
    A.unify Man.mgr x y

  let leq x y =
    (* TODO: float *)
    let x_env = A.env x in
    let y_env = A.env y in
    let (x_vars, _) = Environment.vars x_env in
    if Array.for_all (Environment.mem_var y_env) x_vars then (
      let y' = A.change_environment Man.mgr y x_env false in
      A.is_leq Man.mgr x y'
    )
    else
      false

  let widen x y =
    let x_env = A.env x in
    let y_env = A.env y in
    if Environment.equal x_env y_env then (
      if GobConfig.get_bool "ana.apron.threshold_widening" then (
        if Oct.manager_is_oct Man.mgr then (
          let octmgr = Oct.manager_to_oct Man.mgr in
          let ts = ResettableLazy.force widening_thresholds_apron in
          let x_oct = Oct.Abstract1.to_oct x in
          let y_oct = Oct.Abstract1.to_oct y in
          let r = Oct.widening_thresholds octmgr (Abstract1.abstract0 x_oct) (Abstract1.abstract0 y_oct) ts in
          Oct.Abstract1.of_oct {x_oct with abstract0 = r}
        )
        else (
          let exps = ResettableLazy.force WideningThresholds.exps in
          let module Arg = struct
            let allow_global = true
          end in
          let module Convert = SharedFunctions.Convert (V) (Bounds(Man)) (Arg) (Tracked) in
          (* this implements widening_threshold with Tcons1 instead of Lincons1 *)
          let tcons1s = List.filter_map (fun e ->
              let no_ov = lazy(IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e)) in
              let dummyask = let f (type a) (q : a Queries.t) : a =
                               (* Convert.tcons1_of_cil_exp supports fancy aggressive simplifications of expressions
                                  via querying the context for int constants that replace subexpressions;
                                  we do not have a context here, so we just use a dummy ask replying top all the time *)
                               Queries.Result.top q
                in
                ({ f } : Queries.ask) in
              match Convert.tcons1_of_cil_exp dummyask y y_env e false no_ov with
              | tcons1 when A.sat_tcons Man.mgr y tcons1 ->
                Some tcons1
              | _
              | exception Convert.Unsupported_CilExp _ ->
                None
            ) exps
          in
          let tcons1_earray: Tcons1.earray = {
            array_env = y_env;
            tcons0_array = tcons1s |> List.to_seq |> Seq.map Tcons1.get_tcons0 |> Array.of_seq
          }
          in
          let w = A.widening Man.mgr x y in
          A.meet_tcons_array_with Man.mgr w tcons1_earray;
          w
        )
      )
      else
        A.widening Man.mgr x y
    )
    else
      y (* env increased, just use joined value in y, assuming env doesn't increase infinitely *)

  let widen x y =
    if M.tracing then M.traceli "apron" "widen %a %a" pretty x pretty y;
    let w = widen x y in
    if M.tracing then M.trace "apron" "widen same %B" (equal y w);
    if M.tracing then M.traceu "apron" "-> %a" pretty w;
    w

  let narrow x y =
    let x_env = A.env x in
    let y_env = A.env y in
    if Environment.equal x_env y_env then (
      if Oct.manager_is_oct Man.mgr then (
        let octmgr = Oct.manager_to_oct Man.mgr in
        let x_oct = Oct.Abstract1.to_oct x in
        let y_oct = Oct.Abstract1.to_oct y in
        let r = Oct.narrowing octmgr (Abstract1.abstract0 x_oct) (Abstract1.abstract0 y_oct) in
        Oct.Abstract1.of_oct {env = x_env; abstract0 = r}
      )
      else
        x
    )
    else
      y (* env decreased, can't decrease infinitely *)

  (* TODO: check environments in pretty_diff? *)
end

module type S2 =
(* TODO: ExS3 or better extend RelationDomain.S3 directly?*)
sig
  module Man: Manager
  module V: RV
  include module type of AOps (Tracked) (Man)
  include SLattice with type t = Man.mt A.t

  include S3 with type t = Man.mt A.t and type var = Var.t
end


module D (Man: Manager)=
struct
  module DWO = DWithOps (Man) (DHetero (Man))
  include SharedFunctions.AssertionModule (V) (DWO) (DWO.Arg)
  include DWO
  module Tracked = Tracked
  module Man = Man
end


module OctagonD = D (OctagonManager)

module type S3 =
sig
  include SLattice
  include AOps with type t := t

  module V: RV
  module Tracked: RelationDomain.Tracked
  module Man: Manager

  val assert_inv : Queries.ask -> t -> exp -> bool -> bool Lazy.t -> t
  val eval_int : Queries.ask -> t -> exp -> bool Lazy.t -> Queries.ID.t
end


module D2 (Man: Manager) : S2 with module Man = Man  =
struct
  include D (Man)
  module V = RelationDomain.V

  type marshal = OctagonD.marshal

  let marshal t : Oct.t Abstract0.t * string array =
    let convert_single (a: t): OctagonD.t =
      if Oct.manager_is_oct Man.mgr then
        Oct.Abstract1.to_oct a
      else
        let generator = to_lincons_array a in
        OctagonD.of_lincons_array generator
    in
    OctagonD.marshal @@ convert_single t

  let unmarshal (m: marshal) = Oct.Abstract1.of_oct @@ OctagonD.unmarshal m
end
