open GoblintCil

(** Symbolic (and fully syntactic) expression "lattice". *)
module Exp =
struct
  include CilType.Exp
  (* This type is abstract in the interface because invariant expressions may be optimized for readability but lack implicit casts, etc, which are required to normally use CIL exp-s in Goblint. *)

  let leq _ _ = failwith "ExpLat: leq" (* cannot say for sure, requires general entailment check *)
  let pretty_diff () _ = failwith "ExpLat: pretty_diff" (* irrelevant, no leq *)

  (* join and meet are not idempotent, commutative and associative,
     but it shouldn't be of issue since there's no leq either.
     Would need at least AC-rewriting, if not more, to ensure those symbolically *)
  let join x y = BinOp (LOr, x, y, intType)
  let meet x y = BinOp (LAnd, x, y, intType)
  let widen x y = y
  let narrow = meet

  let to_cil = Fun.id


  module ES = SetDomain.Make (CilType.Exp)

  (* Turns an expression into alist of conjuncts, pulling out common conjuncts from top-level disjunctions *)
  let rec pullOutCommonConjuncts e =
    let rec to_conjunct_set = function
      | Cil.BinOp(LAnd,e1,e2,_) -> ES.join (to_conjunct_set e1) (to_conjunct_set e2)
      | e -> ES.singleton e
    in
    let combine_conjuncts es = ES.fold (fun e acc -> match acc with | None -> Some e | Some acce -> Some (BinOp(LAnd,acce,e,Cil.intType))) es None in
    match e with
    | Cil.BinOp(LOr, e1, e2,t) ->
      let e1s = pullOutCommonConjuncts e1 in
      let e2s = pullOutCommonConjuncts e2 in
      let common = ES.inter e1s e2s in
      let e1s' = ES.diff e1s e2s in
      let e2s' = ES.diff e2s e1s in
      (match combine_conjuncts e1s', combine_conjuncts e2s' with
       | Some e1e, Some e2e -> ES.add (BinOp(LOr,e1e,e2e,Cil.intType)) common
       | _ -> common (* if one of the disjuncts is empty, it is equivalent to true here *)
      )
    | e -> to_conjunct_set e

  let process inv =
    let exp_deep_unroll_types =
      if GobConfig.get_bool "witness.invariant.typedefs" then
        Fun.id
      else
        InvariantCil.exp_deep_unroll_types
    in
    let inv' =
      inv
      |> exp_deep_unroll_types
      |> InvariantCil.exp_replace_original_name
    in
    if GobConfig.get_bool "witness.invariant.split-conjunction" then
      ES.elements (pullOutCommonConjuncts inv')
      |> List.filter (Fun.negate InvariantCil.exp_contains_anon_type)
    else
      [inv']
end


(** Lift {!ExpLat} such that join/meet folds don't introduce excessive [|| 0|] or [&& 1] expressions. *)

module N =
struct
  include Printable.DefaultConf
  let bot_name = "false"
  let top_name = "true"
end

include Lattice.LiftConf (N) (Exp)

let none = top ()
let of_exp = lift

let ( && ) = meet
let ( || ) = join


type context = {
  path: int option;
  lvals: Lval.Set.t;
}

let default_context = {
  path = None;
  lvals = Lval.Set.top ();
}
