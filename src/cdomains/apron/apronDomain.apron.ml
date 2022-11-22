open Prelude
open GoblintCil
open Pretty
(* A binding to a selection of Apron-Domains *)
open Apron
open RelationDomain
open SharedFunctions


module BI = IntOps.BigIntOps

module M = Messages

(** Resources for working with Apron:
    - OCaml API docs: https://antoinemine.github.io/Apron/doc/api/ocaml/index.html
    - C API docs (better function descriptions): https://antoinemine.github.io/Apron/doc/api/c/index.html
    - CEA 2007 slides (overview, mathematical descriptions): https://antoinemine.github.io/Apron/doc/papers/expose_CEA_2007.pdf
    - C API docs PDF (alternative mathematical descriptions): https://antoinemine.github.io/Apron/doc/api/c/apron.pdf
    - heterogeneous environments: https://link.springer.com/chapter/10.1007%2F978-3-030-17184-1_26 (Section 4.1) *)

let widening_thresholds_apron = ResettableLazy.from_fun (fun () ->
    let t = if GobConfig.get_string "ana.apron.threshold_widening_constants" = "comparisons" then WideningThresholds.octagon_thresholds () else WideningThresholds.thresholds_incl_mul2 () in
    let r = List.map (fun x -> Apron.Scalar.of_mpqf @@ Mpqf.of_mpz @@ Z_mlgmpidl.mpz_of_z x) t in
    Array.of_list r
  )

let reset_lazy () =
  ResettableLazy.reset widening_thresholds_apron

module Var = SharedFunctions.Var
module V = RelationDomain.V(Var)


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
  val assign_exp : t -> Var.t -> exp -> bool Lazy.t -> t
  val assign_var : t -> Var.t -> Var.t -> t
  val substitute_exp : t -> Var.t -> exp -> bool Lazy.t -> t
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
  val assign_exp_with : t -> Var.t -> exp -> bool Lazy.t -> unit
  val assign_exp_parallel_with : t -> (Var.t * exp) list -> bool -> unit
  val assign_var_with : t -> Var.t -> Var.t -> unit
  val assign_var_parallel_with : t -> (Var.t * Var.t) list -> unit
  val substitute_exp_with : t -> Var.t -> exp -> bool Lazy.t-> unit
  val substitute_exp_parallel_with :
    t -> (Var.t * exp) list -> bool Lazy.t -> unit
  val substitute_var_with : t -> Var.t -> Var.t -> unit
end

module type AOpsImperativeCopy =
sig
  include AOpsImperative
  val copy : t -> t
end

module type AOpsPT =
sig
  type t
  val remove_vars_pt_with : t -> Var.t list -> t
  val remove_filter_pt_with: t -> (Var.t -> bool) -> t

  val assign_var_parallel_pt_with : t -> (Var.t * Var.t) list -> t

  val copy_pt : t -> t
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
  let assign_exp d v e no_ov =
    let nd = copy d in
    assign_exp_with nd v e no_ov;
    nd
  let assign_var d v v' =
    let nd = copy d in
    assign_var_with nd v v';
    nd
  let substitute_exp d v e no_ov =
    let nd = copy d in
    substitute_exp_with nd v e no_ov;
    nd
end

module AOpsImperativePT(AImp: AOpsImperativeCopy): AOpsPT with type t = AImp.t =
struct
  open AImp
  type nonrec t = t

  let remove_vars_pt_with t vars = remove_vars_with t vars; t
  let remove_filter_pt_with t f = remove_filter_with t f; t
  let assign_var_parallel_pt_with t vars = assign_var_parallel_with t vars; t
  let copy_pt = copy
end

(** Extra functions that don't have the pure-imperative correspondence. *)
module type AOpsExtra =
sig
  type t
  val copy : t -> t
  val vars_as_array : t -> Var.t array
  val vars : t -> Var.t list
  type marshal
  val unmarshal : marshal -> t
  val marshal : t -> marshal
  val mem_var : t -> Var.t -> bool
  val assign_var_parallel' :
    t -> Var.t list -> Var.t list -> t
  val meet_tcons : t -> Tcons1.t -> exp -> t
  val to_lincons_array : t -> Lincons1.earray
  val of_lincons_array : Lincons1.earray -> t

  val cons_to_cil_exp: Lincons1.t -> exp option
  val invariant: t -> Lincons1.t list
end

module type AOps =
sig
  include AOpsExtra
  include AOpsImperative with type t := t
  include AOpsPT with type t := t
  include AOpsPure with type t := t
end


(** Convenience operations on A. *)
module AOps0 (Tracked: Tracked) (Man: Manager) =
struct
  open SharedFunctions
  module Convert = Convert (V) (Bounds(Man)) (struct let allow_global = false end) (Tracked)

  type t = Man.mt A.t

  type var = Var.t

  let env t = A.env t

  let copy = A.copy Man.mgr

  let vars_as_array d =
    let ivs, fvs = Environment.vars (A.env d) in
    assert (Array.length fvs = 0); (* shouldn't ever contain floats *)
    ivs

  let vars d =
    let ivs = vars_as_array d in
    List.of_enum (Array.enum ivs)

  (* marshal type: Abstract0.t and an array of var names *)
  type marshal = Man.mt Abstract0.t * string array

  let unmarshal ((abstract0, vs): marshal): t =
    let vars = Array.map Var.of_string vs in
    (* We do not have real-valued vars, so we pass an empty array in their place. *)
    let env = Environment.make vars [||] in
    {abstract0; env}

  let marshal (x: t): marshal =
    let vars = Array.map Var.to_string (vars_as_array x) in
    x.abstract0, vars

  let mem_var d v = Environment.mem_var (A.env d) v

  let add_vars_with nd vs =
    let env = A.env nd in
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var env v))
      |> Array.of_enum
    in
    let env' = Environment.add env vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let remove_vars_with nd vs =
    let env = A.env nd in
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    let env' = Environment.remove env vs' in
    A.change_environment_with Man.mgr nd env' false

  let remove_filter_with nd f =
    let env = A.env nd in
    let vs' =
      vars nd
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    let env' = Environment.remove env vs' in
    A.change_environment_with Man.mgr nd env' false

  let keep_vars_with nd vs =
    let env = EnvOps.keep_vars (A.env nd) vs in
    A.change_environment_with Man.mgr nd env false

  let keep_filter_with nd f =
    let env = EnvOps.keep_filter (A.env nd) f in
    A.change_environment_with Man.mgr nd env false

  let forget_vars_with nd vs =
    (* Unlike keep_vars_with, this doesn't check mem_var, but assumes valid vars, like assigns *)
    let vs' = Array.of_list vs in
    A.forget_array_with Man.mgr nd vs' false

  let assign_exp_with nd v e no_ov =
    match Convert.texpr1_of_cil_exp nd (A.env nd) e (Lazy.force no_ov) with
    | texpr1 ->
      if M.tracing then M.trace "apron" "assign_exp converted: %s\n" (Format.asprintf "%a" Texpr1.print texpr1);
      A.assign_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp _ ->
      if M.tracing then M.trace "apron" "assign_exp unsupported\n";
      forget_vars_with nd [v]

  let assign_exp d v e no_ov =
    let nd = copy d in
    assign_exp_with nd v e no_ov;
    nd

  let assign_exp_parallel_with nd ves no_ov =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition assigns with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.enum
      |> Enum.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp nd env e no_ov with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp _ -> None
        ))
      |> Enum.partition (fun (_, e_opt) -> Option.is_some e_opt)
    in
    (* parallel assign supported *)
    let (supported_vs, texpr1s) =
      supported
      |> Enum.map (Tuple2.map2 Option.get)
      |> Enum.uncombine
      |> Tuple2.map Array.of_enum Array.of_enum
    in
    A.assign_texpr_array_with Man.mgr nd supported_vs texpr1s None;
    (* forget unsupported *)
    let unsupported_vs =
      unsupported
      |> Enum.map fst
      |> Array.of_enum
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
      |> List.enum
      |> Enum.map (Tuple2.map2 (Texpr1.var env))
      |> Enum.uncombine
      |> Tuple2.map Array.of_enum Array.of_enum
    in
    A.assign_texpr_array_with Man.mgr nd vs texpr1s None

  let assign_var_parallel' d vs v's = (* unpaired parallel assigns *)
    (* TODO: _with version? *)
    let env = A.env d in
    let vs = Array.of_list vs in
    let texpr1s =
      v's
      |> List.enum
      |> Enum.map (Texpr1.var env)
      |> Array.of_enum
    in
    A.assign_texpr_array Man.mgr d vs texpr1s None

  let substitute_exp_with nd v e no_ov =
    match Convert.texpr1_of_cil_exp nd (A.env nd) e (Lazy.force no_ov) with
    | texpr1 ->
      A.substitute_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp _ ->
      forget_vars_with nd [v]

  let substitute_exp_parallel_with nd ves no_ov =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition substitutes with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.enum
      |> Enum.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp nd env e (Lazy.force no_ov) with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp _ -> None
        ))
      |> Enum.partition (fun (_, e_opt) -> Option.is_some e_opt)
    in
    (* parallel substitute supported *)
    let (supported_vs, texpr1s) =
      supported
      |> Enum.map (Tuple2.map2 Option.get)
      |> Enum.uncombine
      |> Tuple2.map Array.of_enum Array.of_enum
    in
    A.substitute_texpr_array_with Man.mgr nd supported_vs texpr1s None;
    (* forget unsupported *)
    let unsupported_vs =
      unsupported
      |> Enum.map fst
      |> Array.of_enum
    in
    A.forget_array_with Man.mgr nd unsupported_vs false

  let substitute_var_with nd v v' =
    (* TODO: non-_with version? *)
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.substitute_texpr_with Man.mgr nd v texpr1 None

  let meet_tcons d tcons1 e =
    let earray = Tcons1.array_make (A.env d) 1 in
    Tcons1.array_set earray 0 tcons1;
    let res = A.meet_tcons_array Man.mgr d earray in
    match Man.name () with
    | "ApronAffEq" ->
      let overflow_res res = if IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e) then res else d in
      begin match Convert.determine_bounds_one_var e with
        | None -> overflow_res res
        | Some (ev, min, max) ->
          let module Bounds = Bounds(Man) in
          let module BI = IntOps.BigIntOps in
          begin match Bounds.bound_texpr res (Convert.texpr1_of_cil_exp res res.env ev true) with
            | Some b_min, Some b_max -> if min = BI.of_int 0 && b_min = b_max then  d
              else if (b_min < min && b_max < min) || (b_max > max && b_min > max) then
                (if GobConfig.get_string "sem.int.signed_overflow" = "assume_none" then A.bottom (A.manager d) (A.env d) else d)
              else res
            | _, _ -> overflow_res res end end
    | _ -> res

  let to_lincons_array d =
    A.to_lincons_array Man.mgr d

  let of_lincons_array (a: Apron.Lincons1.earray) =
    A.of_lincons_array Man.mgr a.array_env a
  let unify (a:t) (b:t) = A.unify Man.mgr a b

  module LinCons = Lincons1
  let cons_to_cil_exp = Convert.cil_exp_of_lincons1
end

module AOps (Tracked: Tracked) (Man: Manager) =
struct
  module AO0 = AOps0 (Tracked) (Man)
  include AO0
  include AOpsPureOfImperative (AO0)
  include AOpsImperativePT(AO0)
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
  type t = Man.mt A.t

  let name () = "Apron " ^ Man.name ()

  (* Functions for bot and top for particular environment. *)
  let top_env = A.top    Man.mgr
  let bot_env = A.bottom Man.mgr
  let is_top_env = A.is_top Man.mgr
  let is_bot_env = A.is_bottom Man.mgr

  let to_yojson x = failwith "TODO implement to_yojson"
  let invariant _ = []
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift x = x

  let show (x:t) =
    Format.asprintf "%a (env: %a)" A.print x (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)
  let pretty () (x:t) = text (show x)

  let equal x y =
    Environment.equal (A.env x) (A.env y) && A.is_eq Man.mgr x y

  let hash (x:t) =
    A.hash Man.mgr x

  let compare (x:t) y: int =
    (* there is no A.compare, but polymorphic compare should delegate to Abstract0 and Environment compare's implemented in Apron's C *)
    Stdlib.compare x y
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nconstraints\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%a" A.print x)) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)))

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

  val invariant: t -> Lincons1Set.elt list
end

module DWithOps (Man: Manager) (D: SLattice with type t = Man.mt A.t) =
struct
  include D
  module Bounds = Bounds (Man)

  include AOps (Tracked) (Man)

  include Tracked

  (** Assert a constraint expression. *)
  let rec assert_cons d e negate (ov: bool Lazy.t) =
    let no_ov = IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e) in
    match e with
    (* Apron doesn't properly meet with DISEQ constraints: https://github.com/antoinemine/apron/issues/37.
       Join Gt and Lt versions instead. *)
    | BinOp (Ne, lhs, rhs, intType) when not negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) negate ov in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) negate ov in
      join assert_gt assert_lt
    | BinOp (Eq, lhs, rhs, intType) when negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) (not negate) ov in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) (not negate) ov in
      join assert_gt assert_lt
    | BinOp (LAnd, lhs, rhs, intType) when not negate ->
      let assert_l = assert_cons d lhs negate ov in
      let assert_r = assert_cons d rhs negate ov in
      meet assert_l assert_r
    | BinOp (LAnd, lhs, rhs, intType) when negate ->
      let assert_l = assert_cons d lhs negate ov in
      let assert_r = assert_cons d rhs negate ov in
      join assert_l assert_r (* de Morgan *)
    | BinOp (LOr, lhs, rhs, intType) when not negate ->
      let assert_l = assert_cons d lhs negate ov in
      let assert_r = assert_cons d rhs negate ov in
      join assert_l assert_r
    | BinOp (LOr, lhs, rhs, intType) when negate ->
      let assert_l = assert_cons d lhs negate ov in
      let assert_r = assert_cons d rhs negate ov in
      meet assert_l assert_r (* de Morgan *)
    | UnOp (LNot,e,_) -> assert_cons d e (not negate) ov
    | _ ->
      begin match Convert.tcons1_of_cil_exp d (A.env d) e negate no_ov with
        | tcons1 ->
          if M.tracing then M.trace "apron" "assert_cons %a %s\n" d_exp e (Format.asprintf "%a" Tcons1.print tcons1);
          if M.tracing then M.trace "apron" "assert_cons st: %a\n" D.pretty d;
          let r = meet_tcons d tcons1 e in
          if M.tracing then M.trace "apron" "assert_cons r: %a\n" D.pretty r;
          r
        | exception Convert.Unsupported_CilExp reason ->
          if M.tracing then M.trace "apron" "assert_cons %a unsupported: %s\n" d_exp e (SharedFunctions.show_unsupported_cilExp reason);
          d
      end

  let invariant x =
    (* Would like to minimize to get rid of multi-var constraints directly derived from one-var constraints,
       but not implemented in Apron at all: https://github.com/antoinemine/apron/issues/44 *)
    (* let x = A.copy Man.mgr x in
       A.minimize Man.mgr x; *)
    let {lincons0_array; array_env}: Lincons1.earray = A.to_lincons_array Man.mgr x in
    Array.enum lincons0_array
    |> Enum.map (fun (lincons0: Lincons0.t) ->
        Lincons1.{lincons0; env = array_env}
      )
    |> List.of_enum
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
    if M.tracing then M.traceli "apron" "strengthening %a\n" pretty j;
    let x_env = A.env x in
    let y_env = A.env y in
    let j_env = A.env j in
    let x_j = A.change_environment Man.mgr x j_env false in
    let y_j = A.change_environment Man.mgr y j_env false in
    let x_cons = A.to_lincons_array Man.mgr x_j in
    let y_cons = A.to_lincons_array Man.mgr y_j in
    let try_add_con j con1 =
      if M.tracing then M.tracei "apron" "try_add_con %s\n" (Format.asprintf "%a" (Lincons1.print: Format.formatter -> Lincons1.t -> unit) con1);
      let t = meet_lincons j con1 in
      let t_x = A.change_environment Man.mgr t x_env false in
      let t_y = A.change_environment Man.mgr t y_env false in
      let leq_x = A.is_leq Man.mgr x t_x in
      let leq_y = A.is_leq Man.mgr y t_y in
      if M.tracing then M.trace "apron" "t: %a\n" pretty t;
      if M.tracing then M.trace "apron" "t_x (leq x %B): %a\n" leq_x pretty t_x;
      if M.tracing then M.trace "apron" "t_y (leq y %B): %a\n" leq_y pretty t_y;
      if leq_x && leq_y then (
        if M.tracing then M.traceu "apron" "added\n";
        t
      )
      else (
        if M.tracing then M.traceu "apron" "not added\n";
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
          Lincons1.iter (fun _ var ->
              if Environment.mem_var env var then
                raise Not_found
            ) con1;
          false
        with Not_found ->
          true
      in
      (* Heuristically reorder constraints to pass 36/12 with singlethreaded->multithreaded mode switching. *)
      (* Put those constraints which strictly are in one argument's env first, to (hopefully) ensure they remain. *)
      let (x_cons1_some_y, x_cons1_only_x) = Array.partition (env_exists_mem_con1 y_env) x_cons1 in
      let (y_cons1_some_x, y_cons1_only_y) = Array.partition (env_exists_mem_con1 x_env) y_cons1 in
      Array.concat [x_cons1_only_x; y_cons1_only_y; x_cons1_some_y; y_cons1_some_x]
    in
    let j = Array.fold_left try_add_con j cons1 in
    if M.tracing then M.traceu "apron" "-> %a\n" pretty j;
    j

  let empty_env = Environment.make [||] [||]

  let bot () =
    top_env empty_env

  let top () =
    failwith "D2.top"

  let is_bot = equal (bot ())
  let is_top _ = false

  let strengthening_enabled = GobConfig.get_bool "ana.apron.strengthening"

  let join x y =
    (* just to optimize joining folds, which start with bot *)
    if is_bot x then
      y
    else if is_bot y then
      x
    else (
      if M.tracing then M.traceli "apron" "join %a %a\n" pretty x pretty y;
      let j = join x y in
      if M.tracing then M.trace "apron" "j = %a\n" pretty j;
      let j =
        if strengthening_enabled then
          strengthening j x y
        else
          j
      in
      if M.tracing then M.traceu "apron" "-> %a\n" pretty j;
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
          let module Convert = SharedFunctions.Convert (V) (Bounds(Man)) (struct let allow_global = true end) (Tracked) in
          (* this implements widening_threshold with Tcons1 instead of Lincons1 *)
          let tcons1s = List.filter_map (fun e ->
              let no_ov = IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e) in
              match Convert.tcons1_of_cil_exp y y_env e false no_ov with
              | tcons1 when A.sat_tcons Man.mgr y tcons1 ->
                Some tcons1
              | _
              | exception Convert.Unsupported_CilExp _ ->
                None
            ) exps
          in
          let tcons1_earray: Tcons1.earray = {
            array_env = y_env;
            tcons0_array = tcons1s |> List.enum |> Enum.map Tcons1.get_tcons0 |> Array.of_enum
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
    if M.tracing then M.traceli "apron" "widen %a %a\n" pretty x pretty y;
    let w = widen x y in
    if M.tracing then M.trace "apron" "widen same %B\n" (equal y w);
    if M.tracing then M.traceu "apron" "-> %a\n" pretty w;
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
(*ToDo ExS3 or better extend RelationDomain.S3 directly?*)
sig
  module Man: Manager
  include module type of AOps (Tracked) (Man)
  include SLattice with type t = Man.mt A.t

  include S3 with type t = Man.mt A.t and type var = Var.t and module LinCons = Lincons1

  val exp_is_cons : exp -> bool
  val assert_cons : t -> exp -> bool -> bool Lazy.t -> t
end


module D (Man: Manager)=
struct
  module DWO = DWithOps (Man) (DHetero (Man))
  include SharedFunctions.AssertionModule (V) (DWO)
  include DWO
  module Tracked = Tracked
  module Man = Man
end

module OctagonD = D (OctagonManager)

module D2 (Man: Manager) : S2 with module Man = Man  =
struct
include D (Man)

type marshal = OctagonD.marshal

let marshal t : Oct.t Abstract0.t * string array =
  (* TODO: why does this duplicate to_oct below? *)
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

module type S3 =
sig
  include SLattice
  include AOps with type t := t

  module Tracked: RelationDomain.Tracked

  val assert_inv : t -> exp -> bool -> bool Lazy.t -> t
  val eval_int : t -> exp -> bool Lazy.t -> Queries.ID.t

  val to_oct: t -> OctagonD.t
end


module D3 (Man: Manager) : S3 =
struct
  include D2 (Man)

  let to_oct (a: t): OctagonD.t =
    if Oct.manager_is_oct Man.mgr then
      Oct.Abstract1.to_oct a
    else
      let generator = to_lincons_array a in
      OctagonD.of_lincons_array generator
end

(** Lift [D] to a non-reduced product with box.
    Both are updated in parallel, but [D] answers to queries.
    Box domain is used to filter out non-relational invariants for output. *)
module BoxProd0 (D: S3) =
struct
  module BoxD = D3 (IntervalManager)

  include Printable.Prod (BoxD) (D)

  let equal (_, d1) (_, d2) = D.equal d1 d2
  let hash (_, d) = D.hash d
  let compare (_, d1) (_, d2) = D.compare d1 d2

  let leq (_, d1) (_, d2) = D.leq d1 d2
  let join (b1, d1) (b2, d2) = (BoxD.join b1 b2, D.join d1 d2)
  let meet (b1, d1) (b2, d2) = (BoxD.meet b1 b2, D.meet d1 d2)
  let widen (b1, d1) (b2, d2) = (BoxD.widen b1 b2, D.widen d1 d2)
  let narrow (b1, d1) (b2, d2) = (BoxD.narrow b1 b2, D.narrow d1 d2)

  let top () = (BoxD.top (), D.top ())
  let bot () = (BoxD.bot (), D.bot ())
  let is_top (_, d) = D.is_top d
  let is_bot (_, d) = D.is_bot d
  let top_env env = (BoxD.top_env env, D.top_env env)
  let bot_env env = (BoxD.bot_env env, D.bot_env env)
  let is_top_env (_, d) = D.is_top_env d
  let is_bot_env (_, d) = D.is_bot_env d
  let unify (b1, d1) (b2, d2) = (BoxD.unify b1 b2, D.unify d1 d2)
  let copy (b, d) = (BoxD.copy b, D.copy d)

  type marshal = BoxD.marshal * D.marshal

  let marshal (b, d) = (BoxD.marshal b, D.marshal d)
  let unmarshal (b, d) = (BoxD.unmarshal b, D.unmarshal d)

  let mem_var (_, d) v = D.mem_var d v
  let vars_as_array (_, d) = D.vars_as_array d
  let vars (_, d) = D.vars d

  let pretty_diff () ((_, d1), (_, d2)) = D.pretty_diff () (d1, d2)

  let add_vars_with (b, d) vs =
    BoxD.add_vars_with b vs;
    D.add_vars_with d vs
  let remove_vars_with (b, d) vs =
    BoxD.remove_vars_with b vs;
    D.remove_vars_with d vs
  let remove_filter_with (b, d) f =
    BoxD.remove_filter_with b f;
    D.remove_filter_with d f
  let keep_filter_with (b, d) f =
    BoxD.keep_filter_with b f;
    D.keep_filter_with d f
  let keep_vars_with (b, d) vs =
    BoxD.keep_vars_with b vs;
    D.keep_vars_with d vs
  let forget_vars_with (b, d) vs =
    BoxD.forget_vars_with b vs;
    D.forget_vars_with d vs
  let assign_exp_with (b, d) v e no_ov =
    BoxD.assign_exp_with b v e no_ov;
    D.assign_exp_with d v e no_ov
  let assign_exp_parallel_with (b, d) ves no_ov =
    BoxD.assign_exp_parallel_with b ves no_ov;
    D.assign_exp_parallel_with d ves no_ov
  let assign_var_with (b, d) v e =
    BoxD.assign_var_with b v e;
    D.assign_var_with d v e
  let assign_var_parallel_with (b, d) vvs =
    BoxD.assign_var_parallel_with b vvs;
    D.assign_var_parallel_with d vvs
  let assign_var_parallel' (b, d) vs v's =
    (BoxD.assign_var_parallel' b vs v's, D.assign_var_parallel' d vs v's)
  let substitute_exp_with (b, d) v e no_ov =
    BoxD.substitute_exp_with b v e no_ov;
    D.substitute_exp_with d v e no_ov
  let substitute_exp_parallel_with (b, d) ves no_ov =
    BoxD.substitute_exp_parallel_with b ves no_ov;
    D.substitute_exp_parallel_with d ves no_ov
  let substitute_var_with (b, d) v1 v2 =
    BoxD.substitute_var_with b v1 v2;
    D.substitute_var_with d v1 v2
  let meet_tcons (b, d) c e = (BoxD.meet_tcons b c e, D.meet_tcons d c e)
  let to_lincons_array (_, d) = D.to_lincons_array d
  let of_lincons_array a = (BoxD.of_lincons_array a, D.of_lincons_array a)

  let cons_to_cil_exp =  D.cons_to_cil_exp
  let assert_inv (b, d) e n no_ov = (BoxD.assert_inv b e n no_ov, D.assert_inv d e n no_ov)
  let eval_int (_, d) = D.eval_int d

  let invariant (b, d) =
    (* diff via lincons *)
    let lcb = D.to_lincons_array (D.of_lincons_array (BoxD.to_lincons_array b)) in (* convert through D to make lincons use the same format *)
    let lcd = D.to_lincons_array d in
    Lincons1Set.(diff (of_earray lcd) (of_earray lcb))
    |> Lincons1Set.elements

  let to_oct (b, d) = D.to_oct d
end

module BoxProd (D: S3): S3 =
struct
  module BP0 = BoxProd0 (D)
  module Tracked = SharedFunctions.Tracked
  include BP0
  include AOpsImperativePT (BP0)
  include AOpsPureOfImperative (BP0)
end
