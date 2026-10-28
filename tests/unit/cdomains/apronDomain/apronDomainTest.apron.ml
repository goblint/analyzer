open GoblintCil
open OUnit2

module ApronDomain = Goblint_lib.ApronDomain
module Queries = Goblint_lib.Queries
module V = Goblint_lib.RelationDomain.V
module D = ApronDomain.D (ApronDomain.PolyhedraManager)

let dummyask : Queries.ask = {Queries.f = Queries.Result.top}

let make_state make_env rhs =
  (* Construct a state satisfying b * 3 = rhs *)
  let b = makeVarinfo false "b" intType in
  let b_exp = Lval (Var b, NoOffset) in
  let relation = BinOp (Eq, BinOp (Mult, b_exp, integer 3, intType), integer rhs, intType) in
  let b_var = V.local b in
  let env = make_env b_var in
  let state_with_relation = D.assert_inv dummyask (D.top_env env) relation false (lazy true) in
  let b_texpr = Apron.Texpr1.of_expr env (Apron.Texpr1.Var b_var) in
  state_with_relation, b_exp, b_texpr

let assert_projection ~bottom ~bounds ~value (state, b_exp, b_texpr) =
  assert_equal
    ~msg:"Apron state is bottom"
    ~printer:string_of_bool bottom (D.is_bot_env state);
  assert_equal
    ~msg:"Integer bounds of b"
    bounds (D.eval_interval dummyask state b_texpr);
  assert_equal
    ~msg:"Integer value of b"
    ~cmp:Queries.ID.equal
    ~printer:(fun value -> if Queries.ID.is_bot_ikind value then "bottom" else Queries.ID.show value)
    value (D.eval_int dummyask state b_exp (lazy true))

let four = Z.of_int 4
let bounds_of_four = (Some (Some four, Some four))

let real_variable _ =
  (* Register b as a real Apron variable while keeping the C expression integer-typed, so
     b = 13/3 survives until Goblint projects the rational bounds to integers. *)
  let b_times_three_equals = make_state (fun b -> Apron.Environment.make [||] [|b|]) in
  assert_projection ~bottom:false ~bounds:bounds_of_four ~value:(Queries.ID.of_int IInt four) (b_times_three_equals 12);
  assert_projection ~bottom:false ~bounds:None ~value:(Queries.ID.bot_of IInt) (b_times_three_equals 13)

let integer_variable _ =
  let b_times_three_equals = make_state (fun b -> Apron.Environment.make [|b|] [||]) in
  assert_projection ~bottom:false ~bounds:bounds_of_four ~value:(Queries.ID.of_int IInt four) (b_times_three_equals 12);
  (* With an integer variable, 3*b = 13 already makes the Apron state bottom. *)
  assert_projection ~bottom:true ~bounds:None ~value:(Queries.ID.bot_of IInt) (b_times_three_equals 13)

let test () =
  "apronDomainTest" >::: [
    "real Apron variable" >:: real_variable;
    "integer Apron variable" >:: integer_variable;
  ]
