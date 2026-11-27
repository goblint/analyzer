(** CIL utilities for relational analyses. *)

open GoblintCil

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module TypeTracked =
struct
  let is_pthread_int_type = function
    | TNamed ({tname = ("pthread_t" | "pthread_key_t" | "pthread_once_t" | "pthread_spinlock_t"); _}, _) -> true (* on Linux these pthread types are integral *)
    | _ -> false

  let type_tracked typ =
    isIntegralType typ && not (is_pthread_int_type typ)
end

(** To be used in relational analyses. *)
module Tracked: Tracked =
struct
  include TypeTracked

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in relation, but just have to be handled separately *)
    let hasTrackAttribute = List.exists (fun (Attr(s,_)) -> s = "goblint_relation_track") in
    type_tracked vi.vtype && (not (GobConfig.get_bool "annotation.goblint_relation_track") || hasTrackAttribute vi.vattr)
end

(** To be used in autotuner. *)
module AutotuneTracked: Tracked =
struct
  include TypeTracked

  let isGoblintStub v = List.exists (fun (Attr(s,_)) -> s = "goblint_stub") v.vattr

  let varinfo_tracked vi = type_tracked vi.vtype && not (isGoblintStub vi) (* must not check for goblint_relation_track because the autotuner wants to add them based on this *)
end

(** Render a linear constraint in signed long long arithmetic, moving negative
    terms to the right when their negation is representable. *)
let exp_of_constraint ~to_varinfo ~pretty_var (constraint_: 'var LinearConstraint.t) =
  let exception Unrepresentable in
  let longlong = TInt (ILongLong, []) in
  let signed_constant i =
    if snd (truncateCilint ILongLong i) <> NoTruncation then raise Unrepresentable;
    let negate = Z.sign i < 0 && snd (truncateCilint ILongLong (Z.neg i)) = NoTruncation in
    negate, Const (CInt ((if negate then Z.neg i else i), ILongLong, None))
  in
  let signed_term (coeff, v) =
    match to_varinfo v with
    | Some vi when IntDomain.Size.is_cast_injective ~from_type:vi.vtype ~to_type:longlong ->
      let var = Cilfacade.mkCast ~kind:Explicit ~e:(Lval (Cil.var vi)) ~newt:longlong in
      if Z.equal coeff Z.one then false, var
      else if Z.equal coeff Z.minus_one then true, var
      else
        let negate, c = signed_constant coeff in
        negate, BinOp (Mult, c, var, longlong)
    | None ->
      Messages.warn ~category:Analyzer "Invariant Apron: cannot convert to cil var: %a" pretty_var v;
      raise Unrepresentable
    | Some _ ->
      Messages.warn ~category:Analyzer "Invariant Apron: cannot convert to cil var in overflow preserving manner: %a" pretty_var v;
      raise Unrepresentable
  in
  let sum = function
    | [] -> Cil.kinteger ILongLong 0
    | (_, first) :: rest -> List.fold_left (fun acc (_, term) -> BinOp (PlusA, acc, term, longlong)) first rest
  in
  try
    let constant = if Z.equal constraint_.constant Z.zero then [] else [signed_constant constraint_.constant] in
    let terms = List.fold_left (fun acc term -> signed_term term :: acc) constant constraint_.terms in
    let negative, positive = List.partition fst terms in
    let op = match constraint_.comparison with
      | Eq -> Eq
      | Ge -> Ge
      | Gt -> Gt
      | Ne -> Ne
    in
    Some (BinOp (op, sum positive, sum negative, intType))
  with Unrepresentable -> None

let invariant_exps ~to_varinfo ~pretty_var invariant_constraints state =
  let include_one_var = GobConfig.get_bool "ana.relation.invariant.one-var" in
  let include_equalities = GobConfig.get_bool "witness.invariant.exact" in
  invariant_constraints ~include_one_var ~include_equalities state
  |> Seq.filter_map (exp_of_constraint ~to_varinfo ~pretty_var)
