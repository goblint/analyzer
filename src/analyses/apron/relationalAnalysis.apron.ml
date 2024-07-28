(** {{!RelationAnalysis} Relational integer value analysis} *)

open Analyses
open RelationalImplementation
open ElinaImplementation

include RelationAnalysis

let spec_module (module RelImpl : Implementation) name: (module MCPSpec) Lazy.t =
  lazy (
    let module Man = (val ApronDomain.get_manager (module RelImpl)) in
    let module AD = ApronDomain.D2 (Man) in
    let diff_box = GobConfig.get_bool "ana.apron.invariant.diff-box" in
    let module AD = (val if diff_box then (module ApronDomain.BoxProd (AD): RelationDomain.RD) else (module AD)) in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (ApronPrecCompareUtil.Util)
      let name () = name
    end
    in
    (module Spec)
  )

let get_spec (module RelImpl : Implementation) name: (module MCPSpec) =
  Lazy.force (spec_module (module RelImpl) name)

let after_config (module RelImpl : Implementation) name =
  let module Spec = (val (get_spec (module RelImpl) name)) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let register =
  Printexc.register_printer
    (function
      | Apron.Manager.Error e ->
        let () = Apron.Manager.print_exclog Format.str_formatter e in
        Some(Printf.sprintf "Apron.Manager.Error\n %s" (Format.flush_str_formatter ()))
      | _ -> None (* for other exceptions *)
    )