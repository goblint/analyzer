open GoblintCil
open Pretty
open Apron

module M = Messages

module Var = SharedFunctions.Var
module V = RelationDomain.V (Var)

module type StringRelationDomain =
sig
  include RelationDomain.RD

  val string_copy: t -> t -> int option -> t
  val string_concat: t -> t -> int option -> t
  val substring_extraction: t -> t -> bool * bool
  val string_comparison: t -> t -> int option -> t
end

module Substring(Val: Lattice.S) : StringRelationDomain =
struct
  module Var = Var
  module V = V
  type var = Var.t

  module Tracked = SharedFunctions.Tracked

  module S = SetDomain.Make (Var * Var) (* TODO: wie mache ich das? *)

  type t = {
    r_set : S.t;
    env : Environment.t
  }
  [@@deriving eq, ord, hash]

  let is_bot_env t = S.is_empty t.r_set

  let vars t = vars t.env

  let add_vars t vars = failwith "TODO"

  let remove_vars t vars = failwith "TODO"

  let remove_vars_with t vars = failwith "TODO"

  let remove_filter t f = failwith "TODO"

  let remove_filter_with t f = failwith "TODO"

  let copy t = failwith "TODO"

  let keep_vars t vars = failwith "TODO"

  let keep_filter t f = failwith "TODO"

  let forget_vars t vars = failwith "TODO"

  let assign_exp t var exp no_ov = failwith "TODO"

  let assign_var t var var' = failwith "TODO"

  let assign_var_parallel_with t varvar' = failwith "TODO"

  let assign_var_parallel' t vars1 vars2 = failwith "TODO"

  let substitute_exp t var exp no_ov = failwith "TODO"

  let unify t1 t2 = failwith "TODO"
  
  let marshal t = failwith "TODO"

  let unmarshal t = failwith "TODO"

  let mem_var t var = failwith "TODO"

  let assert_inv t exp negate no_ov = failwith "TODO"

  let eval_int t exp no_ov = failwith "TODO"

  let cil_exp_of_lincons1 a = failwith "TODO"

  let invariant t = failwith "TODO"

  let string_copy dest src n = failwith "TODO"

  let string_concat dest src n = failwith "TODO"

  let substring_extraction haystack needle = failwith "TODO"

  let string_comparison t1 t2 n = failwith "TODO"
end

