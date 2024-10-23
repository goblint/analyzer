(** {{!RelationAnalysis} Relational integer value analysis} using an OCaml implementation of the affine equalities domain ([affeq]).

    @see <https://doi.org/10.1007/BF00268497> Karr, M. Affine relationships among variables of a program. *)

open Analyses

include RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = AffineEqualityDomain.D2 (VectorMatrix.ArrayVector) (VectorMatrix.ArrayMatrix) in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "affeq"
      let finalize () =
        let (size,n) = !AffineEqualityDomain.sparseity_stats in
        let average = Q.div (Q.of_bigint (Z.mul (Z.of_int 100) (n))) (Q.of_bigint (size)) in
        M.info_noloc ~category:Analyzer "Sparseity %f%% in total: %Ld" (Q.to_float average) (Z.to_int64 size)
    end
    in
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
