(** Ref: Affine Relationships Among Variables of a Program, Michael Karr 1976
  https://link.springer.com/content/pdf/10.1007/BF00268497.pdf *)
open Analyses

include RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = AffineEqualityDomain.D2 in
    let module ADVM = SharedFunctions.AssertionModule (AD(val VectorMatrix.get_vector ()) (val VectorMatrix.get_matrix ())) in
    let module RD: RelationDomain.RD =
    struct
      module Var = SharedFunctions.Var
      module V = RelationDomain.V(Var)
      include ADVM
    end in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec = struct include SpecFunctor (Priv) (RD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "affeq" end in
    (module Spec)
  )

let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let after_config () =
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let _ =
  AfterConfig.register after_config
