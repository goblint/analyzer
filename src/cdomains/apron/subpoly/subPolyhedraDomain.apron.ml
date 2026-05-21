(** OCaml implementation of the subpolyhedra domain.

    @see <https://www.microsoft.com/en-us/research/wp-content/uploads/2011/06/subpolyhedra.pdf>  Subpolyhedra. *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron

module Mpqf = SharedFunctions.Mpqf

(** Variable
 * type t, basically ordered and printable
*)
module type Var = sig
  type t [@@deriving hash]
  val compare : t -> t -> int
  val string_of : t -> string
end

(**
 * SubPoly module
 * - internal representation of a consistent subpolyhedron
 * TODO: pick a type t, maybe (affine-equality, interval map)
 *)
module SubPoly (Var : Var) = struct
  (** Placeholder internal type. Replace with (affeq, interval-map, ...) once designed. *)
  type t = unit [@@deriving eq, ord, hash]

  let copy = Fun.id
  let empty () = ()
  let is_empty _ = failwith "SubPolyhedraDomain.SubPoly.is_empty: not implemented"
  
  (*reuse leanies index shifts to implement dim add and remove.*)
  let dim_add (_ch: Apron.Dim.change) _t = failwith "SubPolyhedraDomain.SubPoly.dim_add: not implemented"
  let dim_remove (_ch: Apron.Dim.change) _t = failwith "SubPolyhedraDomain.SubPoly.dim_remove: not implemented"

  let string_of _ = "<subpoly>"

  (* include meet/join/widen etc. *)
  (*get the subpoly methods implemented here, befor ewiring to oursite world in D*)
  let meet _a _b = failwith "SubPolyhedraDomain.meet: not implemented"
  let leq _a _b = failwith "SubPolyhedraDomain.leq: not implemented"
  let join _a _b = failwith "SubPolyhedraDomain.join: not implemented"
  let widen _a _b = failwith "SubPolyhedraDomain.widen: not implemented"
  let narrow _a _b = failwith "SubPolyhedraDomain.narrow: not implemented"
  let unify _a _b = failwith "SubPolyhedraDomain.unify: not implemented"


  let _ = Var.string_of (* silence unused-functor-arg warning until Var is actually used *)
end

module VarManagement =
struct
  module Str = struct
    type t = string
    let compare = String.compare
    let string_of = Fun.id
    let hash = Hashtbl.hash
  end
  module SubPolyDomain = SubPoly(Str)
  include SharedFunctions.VarManagementOps (SubPolyDomain)

  let dim_add = SubPolyDomain.dim_add
  (*potentially add dim_remove here, not sure though*)  
  let size _t = failwith "SubPolyhedraDomain.size: not implemented"
end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement
  let bound_texpr _t _texpr = failwith "SubPolyhedraDomain.bound_texpr: not implemented"
end

module D =
struct
  include Printable.Std
  include RatOps.ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  let name () = "subpoly"

  let to_yojson _ = failwith "SubPolyhedraDomain.to_yojson: not implemented"

  (* pretty printing *)
  let show t = match t.d with
    | None -> "\tBot\n"
    | Some d -> SubPolyDomain.string_of d
  let pretty () x = text (show x)
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml _f _x = failwith "SubPolyhedraDomain.printXml: not implemented"

  (* basic lattice handling *)
  let top () = failwith "SubPolyhedraDomain.top: not implemented"
  let is_top _ = failwith "SubPolyhedraDomain.is_top: not implemented"
  let is_bot _ = failwith "SubPolyhedraDomain.is_bot: not implemented"

  (* fixpoint iteration handling *)
  let meet _a _b = failwith "SubPolyhedraDomain.meet: not implemented"
  let leq _a _b = failwith "SubPolyhedraDomain.leq: not implemented"
  let join _a _b = failwith "SubPolyhedraDomain.join: not implemented"
  let widen _a _b = failwith "SubPolyhedraDomain.widen: not implemented"
  let narrow _a _b = failwith "SubPolyhedraDomain.narrow: not implemented"
  let unify _a _b = failwith "SubPolyhedraDomain.unify: not implemented"

  (* transfer functions *)
  let forget_var _t _v = failwith "SubPolyhedraDomain.forget_var: not implemented"
  let forget_vars _t _vs = failwith "SubPolyhedraDomain.forget_vars: not implemented"
  let assign_exp _ask _t _var _exp _ = failwith "SubPolyhedraDomain.assign_exp: not implemented"
  let assign_var _t _v _v' = failwith "SubPolyhedraDomain.assign_var: not implemented"
  let assign_var_parallel _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel: not implemented"
  let assign_var_parallel_with _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel_with: not implemented"
  let assign_var_parallel' _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel': not implemented"
  let substitute_exp _ask _t _var _exp _no_ov = failwith "SubPolyhedraDomain.substitute_exp: not implemented"
  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  (* Module AssertionRels demands: *)
  let assert_constraint _ask _d _e _negate (_no_ov: bool Lazy.t) = failwith "SubPolyhedraDomain.assert_constraint: not implemented"
  let env t = t.env
  let eval_interval _ask = Bounds.bound_texpr
  let invariant _t = failwith "SubPolyhedraDomain.invariant: not implemented"

  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t
  let unmarshal t = t
  let relift t = t
end

module D2: RelationDomain.RD with type var = Var.t =
struct
  module D = D
  module ConvArg = struct
    let allow_global = false
  end
  include SharedFunctions.AssertionModule (D.V) (D) (ConvArg)
  include D
end
