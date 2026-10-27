open GoblintCil
open OUnit2

module ApronDomain = Goblint_lib.ApronDomain
module Queries = Goblint_lib.Queries
module V = Goblint_lib.RelationDomain.V
module D = ApronDomain.D (ApronDomain.PolyhedraManager)

let dummyask : Queries.ask = {Queries.f = Queries.Result.top}

let empty_integer_projection _ =
  let b = makeVarinfo false "b" intType in
  let b_exp = Lval (Var b, NoOffset) in
  let relation = BinOp (Eq, BinOp (Mult, integer 3, b_exp, intType), integer 13, intType) in
  let b_var = V.local b in
  let env = Apron.Environment.make [||] [|b_var|] in (* b is a real variable, not an int variable (unlike in ApronDomain) *)
  let state_with_relation = D.assert_inv dummyask (D.top_env env) relation false (lazy true) in
  let actual = D.eval_int dummyask state_with_relation b_exp (lazy true) in
  let expected = Queries.ID.bot_of IInt in
  let show value =
    if Queries.ID.is_bot_ikind value then "bottom" else Queries.ID.show value
  in
  assert_equal
    ~msg:"3 * b = 13 has no integer solution, so evaluating b must return bottom"
    ~cmp:Queries.ID.equal
    ~printer:show
    expected actual

let test () =
  "apronDomainTest" >::: [
    "empty integer projection" >:: empty_integer_projection;
  ]
