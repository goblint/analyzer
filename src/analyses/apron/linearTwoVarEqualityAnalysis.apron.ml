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
      module Var = LinearTwoVarEqualityDomain.Var
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

let after_config () =
  let x1 = Apron.Var.of_string "x1" in
  let x2 = Apron.Var.of_string "x2" in
  let x3 = Apron.Var.of_string "x3" in
  let x4 = Apron.Var.of_string "x4" in
  let x2' = Apron.Var.of_string "x2'" in
  let x1' = Apron.Var.of_string "x1'" in
  let env_test = Apron.Environment.make (Array.of_list [x1; x2; x3; x4]) @@ Array.of_list []  in
  let varM_test = D.top_env env_test in
  print_string @@ D.show varM_test ;
  print_string @@ D.show (D.assign_texpr varM_test x1 (Texpr1.Cst (Coeff.s_of_int 5)));
  failwith "Test completed";
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let _ = 
  AfterConfig.register after_config
