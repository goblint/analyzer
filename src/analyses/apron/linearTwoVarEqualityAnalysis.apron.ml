(** {{!RelationAnalysis} Relational integer value analysis} using an OCaml implementation of the linear two-variable equalities domain ([...]).

    @see <http://doi.acm.org/10.1145/2049706.2049710> A. Flexeder, M. Petter, and H. Seidl Fast Interprocedural Linear Two-Variable Equalities. *)

open Analyses
open Apron
open LinearTwoVarEqualityDomain

include RelationAnalysis

(** TODO: modify code *)
let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = LinearTwoVarEqualityDomain.D2  in
    let module RD: RelationDomain.RD =
    struct
      module V = LinearTwoVarEqualityDomain.V
      include AD
    end
    in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (RD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "lin2vareq"
    end
    in
    (module Spec)
  )

let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let test1 () = 
  let x1 = Apron.Var.of_string "x1" in
  let x2 = Apron.Var.of_string "x2" in
  let x3 = Apron.Var.of_string "x3" in
  let x4 = Apron.Var.of_string "x4" in
  let env_test = Apron.Environment.make (Array.of_list [x1; x2; x3; x4]) @@ Array.of_list []  in
  let varM_test = D.identity env_test in
  let test = D.assign_texpr varM_test x1 (Texpr1.Cst (Coeff.s_of_int 5)) in (*x1 = 5*)
  let test = D.assign_var test x3 x2 in (*x3 = x2*)
  let test = D.assign_var test x4 x2 in (*x4 = x2*)
  let test = D.assign_texpr test x2 (Texpr1.Cst (Coeff.s_of_int 3)) in (*x1 = 5*)
  (*let test = D.assign_texpr test x2 (Texpr1.Cst (Coeff.s_of_int 5)) in (*x2 = 5*)*)
  print_string "Test1:\n";
  print_string "Original variable setup:\n";
  print_string @@ D.show varM_test ;
  print_string "After x1 = 5:\n";
  print_string @@ D.show (D.assign_texpr varM_test x1 (Texpr1.Cst (Coeff.s_of_int 5)));
  print_string "After even more assignments:\n";
  print_string @@ D.show test;
  print_string "Test1 completed\n"

let test2 () = 
  let x1 = Apron.Var.of_string "x1" in
  let x2 = Apron.Var.of_string "x2" in
  let x3 = Apron.Var.of_string "x3" in
  let x4 = Apron.Var.of_string "x4" in
  let x2' = Apron.Var.of_string "x2'" in
  let x1' = Apron.Var.of_string "x1'" in
  let env_test = Apron.Environment.make (Array.of_list [x1; x2; x3; x4]) @@ Array.of_list []  in
  let env_test'  = Apron.Environment.make (Array.of_list [x1'; x2';x2 ]) @@ Array.of_list []  in
  let varM_test = D.identity env_test in
  let varM_test' = D.identity env_test' in
  let test = D.assign_texpr varM_test x1 (Texpr1.Cst (Coeff.s_of_int 5)) in (*x1 = 5*)
  let test = D.assign_var test x3 x2 in (*x3 = x2*)
  let test = D.assign_var test x4 x2 in (*x4 = x2*)
  let test'  = D.assign_texpr varM_test' x1' (Texpr1.Cst (Coeff.s_of_int 3)) in (*x1' = 3*)
  let test'  = D.assign_var test' x2 x1' in (*x2  = x1'*)
  (*let test = D.assign_texpr test x2 (Texpr1.Cst (Coeff.s_of_int 5)) in (*x2 = 5*)*)
  print_string "Test2:\n";
  print_string "Original variable setup:\n";
  print_string @@ D.show varM_test ;
  print_string "After x1 = 5:\n";
  print_string @@ D.show (D.assign_texpr varM_test x1 (Texpr1.Cst (Coeff.s_of_int 5)));
  print_string "After even more assignments:\n";
  print_string @@ D.show test;
  print_string "Other environments:\n";
  print_string @@ D.show test';
  print_string "Meet environments:\n";
  print_string @@ D.show (D.meet test test'); 
  print_string "change_d:\n";
  let sup_env = Environment.lce test.env test'.env in
  print_string @@ D.show (VarManagement.change_d test sup_env true false);
  print_string "reduce_col:\n";
  let sup_env = Environment.lce test.env test'.env in
  let after_change_d = VarManagement.change_d test sup_env true false in
  print_string @@ D.show ({d = Some ((EqualitiesArray.reduce_col (Option.get after_change_d.d) 3)); env = sup_env}); 
  print_string "drop_vars:\n";
  let sup_env = Environment.lce test.env test'.env in
  print_string @@ D.show (VarManagement.drop_vars (VarManagement.change_d test sup_env true false) [x1'; x2'] true); 
  print_string "Test2 completed\n"

  let test3 () = 
    let x = Apron.Var.of_string "x" in
    let y = Apron.Var.of_string "y" in
    let env_test = Apron.Environment.make (Array.of_list [x; y]) @@ Array.of_list []  in
    let varM_test = D.identity env_test in
    let test = D.assign_texpr varM_test x (Texpr1.Cst (Coeff.s_of_int 1)) in (*x = 1*)
    let test = D.assign_texpr test y (Texpr1.Cst (Coeff.s_of_int 1)) in (*y = 1*)
    let tcons1 = Apron.Tcons1.make (Texpr1.of_expr env_test  (Binop (Sub, Var x, Var y, Int, Near ))) EQ in
    print_string "Test1:\n";
    print_string "Original variable setup:\n";
    print_string @@ D.show varM_test ;
    print_string "After x = 1 and y=1:\n";
    print_string @@ D.show test;
    print_string "Tcons:\n";
    print_string @@ D.show @@ D.meet_tcons test tcons1 " ";
    print_string "Test1 completed\n"

let after_config () =(*
  test3();
  failwith "No error Test completed";
*)
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let _ = 
  AfterConfig.register after_config
