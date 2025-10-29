(** {{!RelationAnalysis} Relational integer value analysis} using an OCaml implementation of the linear two-variable equalities domain ([lin2vareq]).

    @see <http://doi.acm.org/10.1145/2049706.2049710> A. Flexeder, M. Petter, and H. Seidl Fast Interprocedural Linear Two-Variable Equalities. *)

open Analyses
include RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = LinearTwoVarEqualityDomain.D2
    in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (RelationPrecCompareUtil.DummyUtil)
      let name () = "lin2vareq"
      let branch man e b =
        if M.tracing then M.trace "lin2vareq" "Branching";
        let res = branch man e b in
        let st = man.local in
        let ask = Analyses.ask_of_man man in
        let _ = assign_from_globals_wrapper ask man.global st e (fun d e' ->
            try
              let tcons = AD.tcons1_of_cil_exp ask d d.env e (not b) (no_overflow ask e) in
              let constraintlist = AD.refine_value_domains man d tcons in
              List.iter (fun e -> man.emit (Events.Assert e))  constraintlist;
              d
            with _  -> d)
        in
        res

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
