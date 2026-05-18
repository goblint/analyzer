(** {{!RelationAnalysis} Relational integer value analysis} using a custom OCaml implementation of the octagons domain ([sparseOctagons]).

    @see <http://doi.acm.org/NOTPUBLISHEDYET> M. Petter, and H. Seidl Sparse Octagons. *)

open Analyses
include RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = SparseOctagonDomain.D2
    in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "sparseOctagons"
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