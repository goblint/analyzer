open Goblint_lib
open OUnit2
open GobApron

module D = PentagonDomain.D
module Subs = Goblint_lib.Subs.Subs
module Pntg = Goblint_lib.Pntg.Pntg
module Intv = Goblint_lib.Intv.Intv

let make_state_with_relation source target =
  let x = Var.of_string "pentagon_x" in
  let y = Var.of_string "pentagon_y" in
  let z = Var.of_string "pentagon_z" in
  let env = Environment.make [|x; y; z|] [||] in
  let source_dim = Environment.dim_of_var env source in
  let target_dim = Environment.dim_of_var env target in
  let y_dim = Environment.dim_of_var env y in
  let z_dim = Environment.dim_of_var env z in
  let state = D.top_of_env env in
  let initial_subs =
    List.init 3 (fun _ -> Subs.VarSet.empty)
    |> Subs.set_value source_dim (Subs.VarSet.singleton target_dim)
    |> Subs.set_value y_dim (Subs.VarSet.singleton z_dim)
  in
  state.d <- Some {Pntg.boxes = List.init 3 (fun _ -> Intv.top ()); subs = initial_subs};
  state, x, y, z, source_dim, target_dim, y_dim, z_dim

let assign_difference state target y z =
  D.assign_texpr state target
    (Texpr1.Binop (Texpr1.Sub, Texpr1.Var y, Texpr1.Var z, Texpr1.Int, Texpr1.Near))

let assert_relation_removed source target =
  let state, x, y, z, source_dim, target_dim, y_dim, z_dim =
    make_state_with_relation source target
  in
  let state = assign_difference state x y z in
  let subs = (Option.get state.d).Pntg.subs in
  assert_bool "relation mentioning the assigned variable was retained"
    (not (Subs.VarSet.mem target_dim (Subs.get_value source_dim subs)));
  assert_bool "unrelated relation should be preserved"
    (Subs.VarSet.mem z_dim (Subs.get_value y_dim subs))

let test_invariant_uses_interval_bounds _ =
  let x = Var.of_string "pentagon_invariant_x" in
  let env = Environment.make [|x|] [||] in
  let state = D.top_of_env env in
  state.d <- Some {
      Pntg.boxes = [Intv.create 2 10];
      subs = [Subs.VarSet.empty];
    };
  let constraints = D.invariant state in
  let constraint_with_coeff coeff =
    List.find (fun constraint_ ->
        Coeff.equal (Lincons1.get_coeff constraint_ x) (Coeff.s_of_int coeff)
      ) constraints
  in
  let lower_bound = constraint_with_coeff 1 in
  let upper_bound = constraint_with_coeff (-1) in
  assert_bool "lower constraint should be x - 2 >= 0"
    (Coeff.equal (Lincons1.get_cst lower_bound) (Coeff.s_of_int (-2)));
  assert_bool "upper constraint should be -x + 10 >= 0"
    (Coeff.equal (Lincons1.get_cst upper_bound) (Coeff.s_of_int 10))

let test_subs_widen_is_upper_bound _ =
  let x_lt_y = Subs.VarSet.singleton 1 in
  let x_lt_y_and_z = Subs.VarSet.of_list [1; 2] in
  let old_state = [x_lt_y; Subs.VarSet.empty; Subs.VarSet.empty] in
  let new_state = [x_lt_y_and_z; Subs.VarSet.empty; Subs.VarSet.empty] in
  let widened = Subs.widen old_state new_state in
  assert_bool "widening must overapproximate its old state"
    (Subs.leq old_state widened);
  assert_bool "widening must overapproximate its new state"
    (Subs.leq new_state widened);
  assert_bool "the common strict bound should be retained"
    (Subs.VarSet.equal (Subs.get_value 0 widened) x_lt_y)

let tests =
  "pentagon-domain" >::: [
    "assignment-removes-outgoing-relation" >:: (fun _ ->
        let x = Var.of_string "pentagon_x" in
        let z = Var.of_string "pentagon_z" in
        assert_relation_removed x z
      );
    "assignment-removes-incoming-relation" >:: (fun _ ->
        let z = Var.of_string "pentagon_z" in
        let x = Var.of_string "pentagon_x" in
        assert_relation_removed z x
      );
    "invariant-uses-interval-bounds" >:: test_invariant_uses_interval_bounds;
    "subs-widen-is-upper-bound" >:: test_subs_widen_is_upper_bound;
  ]
