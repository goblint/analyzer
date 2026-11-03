(** {{!RelationAnalysis} Relational integer value analysis} using an OCaml implementation of the subpolyhedra domain ([subpoly]).

    It features a reduced product of our sparse affine equality implementation with an arbitrary precision rational integer domain.
    We have also implemented
    - reduction via propagation of intervals along linear equalities
    - additional reduction via base exploration with a linear explorer
    - restauration of implicit slack-defining equalities

    @see <https://doi.org/10.1007/978-3-540-93900-9_20>  SubPolyhedra: A (More) Scalable Approach to Infer Linear Inequalities. Laviron & Logozzo, VMCAI 2009 *)

open Analyses
include RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = SubPolyhedraDomain.D2
    in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "subpoly"
    end
    in
    (module Spec)
  )

let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let after_config () =
  let module Spec = (val get_spec ()) in
  MCP.register_analysis ~usesApron:true (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let _ =
  AfterConfig.register after_config
