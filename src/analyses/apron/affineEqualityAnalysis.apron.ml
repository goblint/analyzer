(** {{!RelationAnalysis} Relational integer value analysis} using an OCaml implementation of the affine equalities domain ([affeq]).

    @see <https://doi.org/10.1007/BF00268497> Karr, M. Affine relationships among variables of a program. *)

open Analyses

open SparseVector
open ListMatrix

open ArrayVector
open ArrayMatrix

include RelationAnalysis

(* There are two versions of the affeq domain.
   1. Sparse without side effects
   2. Dense Array with side effects
   Default: sparse implementation
   The array implementation with side effects of the affeq domain is used when the --disable ana.affeq.sparse option is set *)
let get_domain: (module RelationDomain.RD) Lazy.t =
  lazy (
    if GobConfig.get_bool "ana.affeq.sparse" then
      (module AffineEqualityDomain.D2 (SparseVector) (ListMatrix))
    else
      (module AffineEqualityDenseDomain.D2 (ArrayVector) (ArrayMatrix))
  )

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = (val Lazy.force get_domain) in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "affeq"
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
