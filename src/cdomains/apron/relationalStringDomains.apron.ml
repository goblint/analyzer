open GoblintCil
open Pretty
open Apron
open Analyses

module M = Messages

module Var = SharedFunctions.PrintableVar
module V = RelationDomain.V (Var)

module type StringRelationDomain =
sig
  include RelationDomain.RD
  type idx

  val string_copy: t -> t -> int option -> t
  val string_concat: t -> t -> int option -> t
  val to_string_length: t -> idx
  val substring_extraction: t -> t -> bool * bool
  val string_comparison: t -> t -> int option -> t
end

module RelationalSubstring (Idx: IntDomain.Z): StringRelationDomain with type idx = Idx.t =
struct
  module Var = Var
  module V = V

  type var = Var.t
  type idx = Idx.t

  module Tracked = SharedFunctions.Tracked

  (* only track must substring relations *)
  module S = SetDomain.Reverse (SetDomain.ToppedSet (Printable.Prod (Var) (Var)) (struct let topname = "All substrings of each other" end))

  type t = {
    mutable r_set : S.t;
    mutable env : Environment.t
  }
  [@@deriving eq, ord, hash]

  let name () = "RelationalSubstringDomain"

  let show t = "Substrings:" ^ S.fold (fun (v1, v2) acc -> acc ^ " (" ^ Var.show v1 ^ " <= " ^ Var.show v2 ^ ")") t.r_set String.empty
  let pretty () t = text (show t)
  let pretty_diff () (t1, t2) = dprintf "%s: %a not leq %a" (name ()) pretty t1 pretty t2
  let printXml f t = failwith "TODO: don't know what to do"
  let to_yojson _ = failwith "TODO: don't know what to do"
  let tag _ = failwith "no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift t = t

  let bot () = {r_set = S.bot (); env = Environment.make [||] [||]}
  let is_bot t = S.is_bot t.r_set

  let top () = {r_set = S.top (); env = Environment.make [||] [||]}
  let is_top t = S.is_top t.r_set

  let leq t1 t2 = S.leq t1.r_set t2.r_set

  let join t1 t2 = {r_set = S.join t1.r_set t2.r_set; env = Environment.lce t1.env t2.env}
  let meet t1 t2 = {r_set = S.meet t1.r_set t2.r_set; env = Environment.lce t1.env t2.env} (* TODO: do I really want the least common environment here? *)
  let widen = join
  let narrow = meet

  let is_bot_env t = S.is_empty t.r_set

  let vars t = 
    let vars_ar1, vars_ar2 = Environment.vars t.env in
    Array.to_list vars_ar1 @ Array.to_list vars_ar2

  let add_vars t _ = t

  let remove_vars t vars = {r_set = S.filter (fun (x, y) -> not (List.mem x vars) && not (List.mem y vars)) t.r_set; env = t.env}

  let remove_vars_with t vars =
    let t' = remove_vars t vars in
    t.r_set <- t'.r_set

  (* TODO: does filter return true when I should remove a var or when I shouldn't *)
  let remove_filter t f = {r_set = S.filter (fun (x, y) -> not (f x) && not (f y)) t.r_set; env = t.env} 

  let remove_filter_with t f =
    let t' = remove_filter t f in
    t.r_set <- t'.r_set

  let copy t = t

  let keep_vars t vars = {r_set = S.filter (fun (x, y) -> List.mem x vars && List.mem y vars) t.r_set; env = t.env}

  (* TODO: same question as remove_filter *)
  let keep_filter t f = {r_set = S.filter (fun (x, y) -> f x && f y) t.r_set; env = t.env} 

  let forget_vars t vars = failwith "TODO"

  let assign_exp t var exp no_ov = failwith "TODO"

  let assign_var t var var' = failwith "TODO"

  let assign_var_parallel_with t varvar' = failwith "TODO"

  let assign_var_parallel' t vars1 vars2 = failwith "TODO"

  let substitute_exp t var exp no_ov = failwith "TODO"

  let unify t1 t2 = meet t1 t2 (* TODO: is this correct? That's what Martin is doing *)

  type marshal = unit
  
  let marshal _ = ()

  let unmarshal () = top ()

  let mem_var t var = Environment.mem_var t.env var

  let assert_inv t exp negate no_ov = failwith "TODO: ?remove from signature?"

  let eval_int t exp no_ov = failwith "TODO: remove from signature"

  let cil_exp_of_lincons1 a = failwith "TODO: remove from signature"

  let invariant t = []

  (* string functions *)
  let string_copy dest src n = failwith "TODO"

  let string_concat dest src n = failwith "TODO"

  let to_string_length t = failwith "TODO"

  let substring_extraction haystack needle = failwith "TODO"

  let string_comparison t1 t2 n = failwith "TODO"
end

