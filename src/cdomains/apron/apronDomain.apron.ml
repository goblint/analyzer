open Prelude
open Cil
open Pretty
(* A binding to a selection of Apron-Domains *)
open Apron
open RelationDomain

module BI = IntOps.BigIntOps

module M = Messages

(** Resources for working with Apron:
    - OCaml API docs: https://antoinemine.github.io/Apron/doc/api/ocaml/index.html
    - C API docs (better function descriptions): https://antoinemine.github.io/Apron/doc/api/c/index.html
    - CEA 2007 slides (overview, mathematical descriptions): https://antoinemine.github.io/Apron/doc/papers/expose_CEA_2007.pdf
    - C API docs PDF (alternative mathematical descriptions): https://antoinemine.github.io/Apron/doc/api/c/apron.pdf
    - heterogeneous environments: https://link.springer.com/chapter/10.1007%2F978-3-030-17184-1_26 (Section 4.1) *)

let widening_thresholds_apron = ResettableLazy.from_fun (fun () ->
  let t = WideningThresholds.thresholds_incl_mul2 () in
  let r = List.map (fun x -> Apron.Scalar.of_mpqf @@ Mpqf.of_mpz @@ Z_mlgmpidl.mpz_of_z x) t in
  Array.of_list r
)

let reset_lazy () =
  ResettableLazy.reset widening_thresholds_apron

module Var =
struct
  include Var
  let equal x y = Var.compare x y = 0
end

module type VarMetadata =
sig
  type t
  val var_name: t -> string
end

module VarMetadataTbl (VM: VarMetadata) =
struct
  module VH = Hashtbl.Make (Var)

  let vh = VH.create 113

  let make_var ?name metadata =
    let name = Option.default_delayed (fun () -> VM.var_name metadata) name in
    let var = Var.of_string name in
    VH.replace vh var metadata;
    var

  let find_metadata var =
    VH.find_option vh var
end

module VM =
struct
  type t =
    | Local (** Var for function local variable (or formal argument). *) (* No varinfo because local Var with the same name may be in multiple functions. *)
    | Arg (** Var for function formal argument entry value. *) (* No varinfo because argument Var with the same name may be in multiple functions. *)
    | Return (** Var for function return value. *)
    | Global of varinfo

  let var_name = function
    | Local -> failwith "var_name of Local"
    | Arg -> failwith "var_name of Arg"
    | Return -> "#ret"
    | Global g -> g.vname
end

module V =
struct
  include VarMetadataTbl (VM)
  open VM

  let local x = make_var ~name:x.vname Local
  let arg x = make_var ~name:(x.vname ^ "'") Arg (* TODO: better suffix, like #arg *)
  let return = make_var Return
  let global g = make_var (Global g)

  let to_cil_varinfo fundec v =
    match find_metadata v with
    | Some (Global v) -> Some v
    | Some (Local) ->
      let vname = Var.to_string v in
      List.find_opt (fun v -> v.vname = vname) (fundec.sformals @ fundec.slocals)
    | _ -> None
end

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
       "polyhedra", (module PolyhedraManager: Manager)]
    in
    let domain = (GobConfig.get_string "ana.apron.domain") in
    match List.assoc_opt domain options with
    | Some man -> man
    | None -> failwith @@ "Apron domain " ^ domain ^ " is not supported. Please check the ana.apron.domain setting."
  )

let get_manager (): (module Manager) =
  Lazy.force manager

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

(* Generic operations on abstract values at level 1 of interface, there is also Abstract0 *)
module A = Abstract1

let int_of_scalar ?round (scalar: Scalar.t) =
  if Scalar.is_infty scalar <> 0 then (* infinity means unbounded *)
    None
  else
    match scalar with
    | Float f -> (* octD, boxD *)
      (* bound_texpr on bottom also gives Float even with MPQ *)
      let f_opt = match round with
        | Some `Floor -> Some (Float.floor f)
        | Some `Ceil -> Some (Float.ceil f)
        | None when Stdlib.Float.is_integer f-> Some f
        | None -> None
      in
      Option.map (fun f -> BI.of_bigint (Z.of_float f)) f_opt
    | Mpqf scalar -> (* octMPQ, boxMPQ, polkaMPQ *)
      let n = Mpqf.get_num scalar in
      let d = Mpqf.get_den scalar in
      let z_opt =
        if Mpzf.cmp_int d 1 = 0 then (* exact integer (denominator 1) *)
          Some n
        else
          begin match round with
            | Some `Floor -> Some (Mpzf.fdiv_q n d) (* floor division *)
            | Some `Ceil -> Some (Mpzf.cdiv_q n d) (* ceiling division *)
            | None -> None
          end
      in
      Option.map Z_mlgmpidl.z_of_mpzf z_opt
    | _ ->
      failwith ("int_of_scalar: unsupported: " ^ Scalar.to_string scalar)

module Bounds (Man: Manager) =
struct
  let bound_texpr d texpr1 =
    let bounds = A.bound_texpr Man.mgr d texpr1 in
    let min = int_of_scalar ~round:`Ceil bounds.inf in
    let max = int_of_scalar ~round:`Floor bounds.sup in
    (min, max)
end

(** Conversion from CIL expressions to Apron. *)
module Convert (Tracked: Tracked) (Man: Manager)=
struct
  open Texpr1
  open Tcons1
  module Bounds = Bounds(Man)
  exception Unsupported_CilExp
  exception Unsupported_Linexpr1



  let texpr1_expr_of_cil_exp d env =
    (* recurse without env argument *)
    let rec texpr1_expr_of_cil_exp = function
      | Lval (Var v, NoOffset) when Tracked.varinfo_tracked v ->
        if not v.vglob then
          let var = Var.of_string v.vname in
          if Environment.mem_var env var then
            Var var
          else
            raise Unsupported_CilExp
        else
          failwith "texpr1_expr_of_cil_exp: globals must be replaced with temporary locals"
      | Const (CInt (i, _, _)) ->
        Cst (Coeff.s_of_mpqf (Mpqf.of_mpz (Z_mlgmpidl.mpz_of_z i)))
      | exp ->
        let expr =
          match exp with
          | UnOp (Neg, e, _) ->
            Unop (Neg, texpr1_expr_of_cil_exp e, Int, Near)
          | BinOp (PlusA, e1, e2, _) ->
            Binop (Add, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
          | BinOp (MinusA, e1, e2, _) ->
            Binop (Sub, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
          | BinOp (Mult, e1, e2, _) ->
            Binop (Mul, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
          | BinOp (Div, e1, e2, _) ->
            Binop (Div, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Zero)
          | BinOp (Mod, e1, e2, _) ->
            Binop (Mod, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
          | CastE (TInt _ as t, e) when IntDomain.Size.is_cast_injective ~from_type:(Cilfacade.typeOf e) ~to_type:t -> (* TODO: unnecessary cast check due to overflow check below? or maybe useful in general to also assume type bounds based on argument types? *)
            Unop (Cast, texpr1_expr_of_cil_exp e, Int, Zero) (* TODO: what does Apron Cast actually do? just for floating point and rounding? *)
          | _ ->
            raise Unsupported_CilExp
        in
        let ik = Cilfacade.get_ikind_exp exp in
        if not (IntDomain.should_ignore_overflow ik) then (
          let (type_min, type_max) = IntDomain.Size.range ik in
          let texpr1 = Texpr1.of_expr env expr in
          match Bounds.bound_texpr d texpr1 with
          | Some min, Some max when BI.compare type_min min <= 0 && BI.compare max type_max <= 0 -> ()
          | _ ->
            (* ignore (Pretty.printf "apron may overflow %a\n" dn_exp exp); *)
            raise Unsupported_CilExp
        );
        expr
    in
    texpr1_expr_of_cil_exp

  let texpr1_of_cil_exp d env e =
    let e = Cil.constFold false e in
    of_expr env (texpr1_expr_of_cil_exp d env e)

  let tcons1_of_cil_exp d env e negate =
    let e = Cil.constFold false e in
    let (texpr1_plus, texpr1_minus, typ) =
      match e with
      | BinOp (r, e1, e2, _) ->
        let texpr1_1 = texpr1_expr_of_cil_exp d env e1 in
        let texpr1_2 = texpr1_expr_of_cil_exp d env e2 in
        (* Apron constraints always compare with 0 and only have comparisons one way *)
        begin match r with
          | Lt -> (texpr1_2, texpr1_1, SUP)   (* e1 < e2   ==>  e2 - e1 > 0  *)
          | Gt -> (texpr1_1, texpr1_2, SUP)   (* e1 > e2   ==>  e1 - e2 > 0  *)
          | Le -> (texpr1_2, texpr1_1, SUPEQ) (* e1 <= e2  ==>  e2 - e1 >= 0 *)
          | Ge -> (texpr1_1, texpr1_2, SUPEQ) (* e1 >= e2  ==>  e1 - e2 >= 0 *)
          | Eq -> (texpr1_1, texpr1_2, EQ)    (* e1 == e2  ==>  e1 - e2 == 0 *)
          | Ne -> (texpr1_1, texpr1_2, DISEQ) (* e1 != e2  ==>  e1 - e2 != 0 *)
          | _ -> raise Unsupported_CilExp
        end
      | _ -> raise Unsupported_CilExp
    in
    let inverse_typ = function
      | EQ -> DISEQ
      | DISEQ -> EQ
      | SUPEQ -> SUP
      | SUP -> SUPEQ
      | EQMOD _ -> failwith "tcons1_of_cil_exp: cannot invert EQMOD"
    in
    let (texpr1_plus, texpr1_minus, typ) =
      if negate then
        (texpr1_minus, texpr1_plus, inverse_typ typ)
      else
        (texpr1_plus, texpr1_minus, typ)
    in
    let texpr1' = Binop (Sub, texpr1_plus, texpr1_minus, Int, Near) in
    Tcons1.make (Texpr1.of_expr env texpr1') typ

  let cil_exp_of_linexpr1 fundec (linexpr1:Linexpr1.t) =
    let longlong = TInt(ILongLong,[]) in
    let coeff_to_const consider_flip (c:Coeff.union_5) = match c with
      | Scalar c ->
        (match int_of_scalar c with
         | Some i ->
           let ci,truncation = truncateCilint ILongLong i in
           if truncation = NoTruncation then
             if not consider_flip || Z.compare i Z.zero >= 0 then
               Const (CInt(i,ILongLong,None)), false
             else
               (* attempt to negate if that does not cause an overflow *)
               let cneg, truncation = truncateCilint ILongLong (Z.neg i) in
               if truncation = NoTruncation then
                 Const (CInt((Z.neg i),ILongLong,None)), true
               else
                 Const (CInt(i,ILongLong,None)), false
           else
             (M.warn ~category:Analyzer "Invariant Apron: coefficient is not int: %s" (Scalar.to_string c); raise Unsupported_Linexpr1)
         | None -> raise Unsupported_Linexpr1)
      | _ -> raise Unsupported_Linexpr1
    in
    let expr = ref (fst @@ coeff_to_const false (Linexpr1.get_cst linexpr1)) in
    let append_summand (c:Coeff.union_5) v =
      match V.to_cil_varinfo fundec v with
      | Some vinfo ->
        (* TODO: What to do with variables that have a type that cannot be stored into ILongLong to avoid overflows? *)
        let var = Cil.mkCast ~e:(Lval(Var vinfo,NoOffset)) ~newt:longlong in
        let coeff, flip = coeff_to_const true c in
        let prod = BinOp(Mult, coeff, var, longlong) in
        if flip then
          expr := BinOp(MinusA,!expr,prod,longlong)
        else
          expr := BinOp(PlusA,!expr,prod,longlong)
      | None -> M.warn ~category:Analyzer "Invariant Apron: cannot convert to cil var: %s"  (Var.to_string v); raise Unsupported_Linexpr1
    in
    Linexpr1.iter append_summand linexpr1;
    !expr


  let cil_exp_of_lincons1 fundec (lincons1:Lincons1.t) =
    let zero = Cil.kinteger ILongLong 0 in
    try
      let linexpr1 = Lincons1.get_linexpr1 lincons1 in
      let cilexp = cil_exp_of_linexpr1 fundec linexpr1 in
      match Lincons1.get_typ lincons1 with
      | EQ -> Some (Cil.constFold false @@ BinOp(Eq, cilexp, zero, TInt(IInt,[])))
      | SUPEQ -> Some (Cil.constFold false @@ BinOp(Ge, cilexp, zero, TInt(IInt,[])))
      | SUP -> Some (Cil.constFold false @@ BinOp(Gt, cilexp, zero, TInt(IInt,[])))
      | DISEQ -> Some (Cil.constFold false @@ BinOp(Ne, cilexp, zero, TInt(IInt,[])))
      | EQMOD _ -> None
    with
      Unsupported_Linexpr1 -> None
end


(** Convenience operations on A. *)
module AOps (Tracked: Tracked) (Man: Manager) =
struct
  module Convert = Convert (Tracked) (Man)

  type t = Man.mt A.t

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

  let add_vars d vs =
    let nd = copy d in
    add_vars_with nd vs;
    nd

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

  let remove_vars d vs =
    let nd = copy d in
    remove_vars_with nd vs;
    nd

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

  let remove_filter d f =
    let nd = copy d in
    remove_filter_with nd f;
    nd

  let keep_vars_with nd vs =
    let env = A.env nd in
    (* Instead of iterating over all vars in env and doing a linear lookup in vs just to remove them,
       make a new env with just the desired vs. *)
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    let env' = Environment.make vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let keep_vars d vs =
    let nd = copy d in
    keep_vars_with nd vs;
    nd

  let keep_filter_with nd f =
    (* Instead of removing undesired vars,
       make a new env with just the desired vars. *)
    let vs' =
      vars nd
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    let env' = Environment.make vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let keep_filter d f =
    let nd = copy d in
    keep_filter_with nd f;
    nd

  let forget_vars_with nd vs =
    (* Unlike keep_vars_with, this doesn't check mem_var, but assumes valid vars, like assigns *)
    let vs' = Array.of_list vs in
    A.forget_array_with Man.mgr nd vs' false

  let forget_vars d vs =
    let nd = copy d in
    forget_vars_with nd vs;
    nd

  let assign_exp_with nd v e =
    match Convert.texpr1_of_cil_exp nd (A.env nd) e with
    | texpr1 ->
      A.assign_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp ->
      forget_vars_with nd [v]

  let assign_exp d v e =
    let nd = copy d in
    assign_exp_with nd v e;
    nd

  let assign_exp_parallel_with nd ves =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition assigns with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.enum
      |> Enum.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp nd env e with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp -> None
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

  let assign_var d v v' =
    let nd = copy d in
    assign_var_with nd v v';
    nd

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

  let substitute_exp_with nd v e =
    match Convert.texpr1_of_cil_exp nd (A.env nd) e with
    | texpr1 ->
      A.substitute_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp ->
      forget_vars_with nd [v]

  let substitute_exp d v e =
    let nd = copy d in
    substitute_exp_with nd v e;
    nd

  let substitute_exp_parallel_with nd ves =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition substitutes with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.enum
      |> Enum.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp nd env e with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp -> None
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

  let meet_tcons d tcons1 =
    let earray = Tcons1.array_make (A.env d) 1 in
    Tcons1.array_set earray 0 tcons1;
    A.meet_tcons_array Man.mgr d earray

  let to_lincons_array d =
    A.to_lincons_array Man.mgr d

  let of_lincons_array (a: Apron.Lincons1.earray) =
    A.of_lincons_array Man.mgr a.array_env a
    let unify (a:t) (b:t) = A.unify Man.mgr a b
end


module type SPrintable =
sig
  include Printable.S
  (* Functions for bot and top for particular environment. *)
  val top_env: Environment.t -> t
  val bot_env: Environment.t -> t
  val is_top_env: t -> bool
  val is_bot_env: t -> bool
end

module DBase (Man: Manager): SPrintable with type t = Man.mt A.t =
struct
  type t = Man.mt A.t

  let name () = "Apron"

  (* Functions for bot and top for particular environment. *)
  let top_env = A.top    Man.mgr
  let bot_env = A.bottom Man.mgr
  let is_top_env = A.is_top Man.mgr
  let is_bot_env = A.is_bottom Man.mgr

  let to_yojson x = failwith "TODO implement to_yojson"
  let invariant _ _ = Invariant.none
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
end


module type SLattice =
sig
  include SPrintable
  include Lattice.S with type t := t
end

module DWithOps (Man: Manager) (D: SLattice with type t = Man.mt A.t) =
struct
  include D
  module Bounds = Bounds (Man)

  module Tracked =
  struct
    let type_tracked typ =
      isIntegralType typ

    let varinfo_tracked vi =
      (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
      type_tracked vi.vtype && not vi.vaddrof
  end

  include AOps (Tracked) (Man)

  include Tracked

  let rec exp_is_cons = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    | UnOp (LNot,e,_) -> exp_is_cons e
    (* expression *)
    | _ -> false

  (** Assert a constraint expression. *)
  let rec assert_cons d e negate =
    match e with
    (* Apron doesn't properly meet with DISEQ constraints: https://github.com/antoinemine/apron/issues/37.
       Join Gt and Lt versions instead. *)
    | BinOp (Ne, lhs, rhs, intType) when not negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) negate in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) negate in
      join assert_gt assert_lt
    | BinOp (Eq, lhs, rhs, intType) when negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) (not negate) in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) (not negate) in
      join assert_gt assert_lt
    | UnOp (LNot,e,_) -> assert_cons d e (not negate)
    | _ ->
      begin match Convert.tcons1_of_cil_exp d (A.env d) e negate with
        | tcons1 ->
          meet_tcons d tcons1
        | exception Convert.Unsupported_CilExp ->
          d
      end

  (** Assert any expression. *)
  let assert_inv d e negate =
    let e' =
      if exp_is_cons e then
        e
      else
        (* convert non-constraint expression, such that we assert(e != 0) *)
        BinOp (Ne, e, zero, intType)
    in
    assert_cons d e' negate

  let check_assert d e =
    if is_bot_env (assert_inv d e false) then
      `False
    else if is_bot_env (assert_inv d e true) then
      `True
    else
      `Top

  (** Evaluate non-constraint expression as interval. *)
  let eval_interval_expr d e =
    match Convert.texpr1_of_cil_exp d (A.env d) e with
    | texpr1 ->
      Bounds.bound_texpr d texpr1
    | exception Convert.Unsupported_CilExp ->
      (None, None)

  (** Evaluate constraint or non-constraint expression as integer. *)
  let eval_int d e =
    let module ID = Queries.ID in
    let ik = Cilfacade.get_ikind_exp e in
    if exp_is_cons e then
      match check_assert d e with
      | `True -> ID.of_bool ik true
      | `False -> ID.of_bool ik false
      | `Top -> ID.top_of ik
    else
      match eval_interval_expr d e with
      | (Some min, Some max) -> ID.of_interval ik (min, max)
      | (Some min, None) -> ID.starting ik min
      | (None, Some max) -> ID.ending ik max
      | (None, None) -> ID.top_of ik

  let invariant (ctx:Invariant.context) x =
    let r = A.to_lincons_array Man.mgr x in
    let cons, env = r.lincons0_array, r.array_env in
    let cons = Array.to_list cons in
    let filter_out_one_var_constraints = false in
    let convert_one (constr:Lincons0.t) =
      if filter_out_one_var_constraints && Linexpr0.get_size (constr.linexpr0) < 2 then
        None
      else
        Convert.cil_exp_of_lincons1 ctx.scope {lincons0=constr; env=env}
    in
    let cil_cons = List.filter_map convert_one cons in
    List.fold_left (fun acc x -> Invariant.((&&) acc (of_exp x))) None cil_cons
end


module DLift (Man: Manager): SLattice with type t = Man.mt A.t =
struct
  include DBase (Man)

  let lift_var = Var.of_string "##LIFT##"

  (** Environment (containing a unique variable [lift_var]) only used for lifted bot and top. *)
  let lift_env = Environment.make [|lift_var|] [||]

  (* Functions for lifted bot and top to implement [Lattice.S]. *)
  let top () = top_env lift_env
  let bot () = bot_env lift_env
  let is_top x = Environment.equal (A.env x) lift_env && is_top_env x
  let is_bot x = Environment.equal (A.env x) lift_env && is_bot_env x

  (* Apron can not join two abstract values have different environments.
     That hapens when we do a join with dead code and for that reason we need
     to handle joining with bottom manually.
     A similar if-based structure with is_top and is_bottom is also there for:
     meet, widen, narrow, equal, leq.*)

  let join x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else (
      if M.tracing then M.tracel "apron" "join %a %a\n" pretty x pretty y;
      A.join (Man.mgr) x y
      (* TODO: return lifted top if different environments? and warn? *)
    )

  let meet x y =
    if is_top x then y else
    if is_top y then x else
      A.meet Man.mgr x y
      (* TODO: return lifted bot if different environments? and warn? *)

  let widen x y =
    if is_bot x then
      y
    else if is_bot y then
      x (* TODO: is this right? *)
    else
      A.widening (Man.mgr) x y
      (* TODO: return lifted top if different environments? and warn? *)

  let narrow = meet

  let leq x y =
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else (
      if M.tracing then M.tracel "apron" "leq %a %a\n" pretty x pretty y;
      Environment.equal (A.env x) (A.env y) && A.is_leq (Man.mgr) x y
      (* TODO: warn if different environments? *)
    )

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module D (Man: Manager) = DWithOps (Man) (DLift (Man))


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
    if Environment.equal x_env y_env  then
      if GobConfig.get_bool "ana.apron.threshold_widening" && Oct.manager_is_oct Man.mgr then
        let octmgr = Oct.manager_to_oct Man.mgr in
        let ts = ResettableLazy.force widening_thresholds_apron in
        let x_oct = Oct.Abstract1.to_oct x in
        let y_oct = Oct.Abstract1.to_oct y in
        let r = Oct.widening_thresholds octmgr (Abstract1.abstract0 x_oct) (Abstract1.abstract0 y_oct) ts in
        Oct.Abstract1.of_oct {x_oct with abstract0 = r}
      else
        A.widening Man.mgr x y
    else
      y (* env increased, just use joined value in y, assuming env doesn't increase infinitely *)

  let widen x y =
    if M.tracing then M.traceli "apron" "widen %a %a\n" pretty x pretty y;
    let w = widen x y in
    if M.tracing then M.trace "apron" "widen same %B\n" (equal y w);
    if M.tracing then M.traceu "apron" "-> %a\n" pretty w;
    w

  (* TODO: better narrow *)
  let narrow x y = x

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module type S2 =
sig
  module Man: Manager
  module Tracked : Tracked
  module Bounds : module type of Bounds (Man)
  include module type of AOps (Tracked) (Man)
  include Tracked
  include SLattice with type t = Man.mt A.t


  val assert_inv : t -> exp -> bool -> t
  val eval_int : t -> exp -> Queries.ID.t
end

type ('a, 'b) aproncomponents_t = { apr : 'a; priv : 'b; } [@@deriving eq, ord, hash, to_yojson]

module D2 (Man: Manager) : S2 with module Man = Man =
struct
  type var = Var.t
  include DWithOps (Man) (DHetero (Man))
  module Man = Man
<<<<<<< HEAD
end

module ApronComponents (D2: S2) (PrivD: Lattice.S):
sig
  module AD: S2 with type Man.mt = D2.Man.mt
  include Lattice.S with type t = (D2.t, PrivD.t) aproncomponents_t
end =
struct
  module AD = D2
  type t = (D2.t, PrivD.t) aproncomponents_t [@@deriving eq, ord, hash, to_yojson]

  include Printable.Std
  open Pretty

  let show r =
    let first  = D2.show r.apr in
    let third  = PrivD.show r.priv in
    "(" ^ first ^ ", " ^ third  ^ ")"

  let pretty () r =
    text "(" ++
    D2.pretty () r.apr
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (D2.name ())) D2.printXml r.apr (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

  let name () = D2.name () ^ " * " ^ PrivD.name ()

  let invariant c {apr; priv} =
    Invariant.(D2.invariant c apr && PrivD.invariant c priv)

  let of_tuple(apr, priv):t = {apr; priv}
  let to_tuple r = (r.apr, r.priv)

  let arbitrary () =
    let tr = QCheck.pair (D2.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = {apr = D2.bot (); priv = PrivD.bot ()}
  let is_bot {apr; priv} = D2.is_bot apr && PrivD.is_bot priv
  let top () = {apr = D2.top (); priv = PrivD.bot ()}
  let is_top {apr; priv} = D2.is_top apr && PrivD.is_top priv

  let leq {apr=x1; priv=x3 } {apr=y1; priv=y3} =
    D2.leq x1 y1 && PrivD.leq x3 y3

  let pretty_diff () (({apr=x1; priv=x3}:t),({apr=y1; priv=y3}:t)): Pretty.doc =
    if not (D2.leq x1 y1) then
      D2.pretty_diff () (x1,y1)
    else
      PrivD.pretty_diff () (x3,y3)

  let op_scheme op1 op3 {apr=x1; priv=x3} {apr=y1; priv=y3}: t =
    {apr = op1 x1 y1; priv = op3 x3 y3 }
  let join = op_scheme D2.join PrivD.join
  let meet = op_scheme D2.meet PrivD.meet
  let widen = op_scheme D2.widen PrivD.widen
  let narrow = op_scheme D2.narrow PrivD.narrow
end
<<<<<<< HEAD
=======


module type VarMetadata =
sig
  type t
  val var_name: t -> string
end

module VarMetadataTbl (VM: VarMetadata) =
struct
  module VH = Hashtbl.Make (Var)

  let vh = VH.create 113

  let make_var ?name metadata =
    let name = Option.default_delayed (fun () -> VM.var_name metadata) name in
    let var = Var.of_string name in
    VH.replace vh var metadata;
    var

  let find_metadata var =
    VH.find_option vh var
end

module VM =
struct
  type t =
    | Local (** Var for function local variable (or formal argument). *) (* No varinfo because local Var with the same name may be in multiple functions. *)
    | Arg (** Var for function formal argument entry value. *) (* No varinfo because argument Var with the same name may be in multiple functions. *)
    | Return (** Var for function return value. *)
    | Global of varinfo

  let var_name = function
    | Local -> failwith "var_name of Local"
    | Arg -> failwith "var_name of Arg"
    | Return -> "#ret"
    | Global g -> g.vname
end

module V =
struct
  include VarMetadataTbl (VM)
  open VM

  let local x = make_var ~name:x.vname Local
  let arg x = make_var ~name:(x.vname ^ "'") Arg (* TODO: better suffix, like #arg *)
  let return = make_var Return
  let global g = make_var (Global g)
end
=======
end
>>>>>>> Add common interface for apron + new domain
>>>>>>> 4da71d5e1 (Add common interface for apron + new domain)
