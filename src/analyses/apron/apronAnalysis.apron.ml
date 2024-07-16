(** {{!RelationAnalysis} Relational integer value analysis} using {!Apron} domains ([apron]). *)

open Analyses

include RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module RelImpl = (val ApronDomain.get_implementation "elina") in
    let module Man = (val ApronDomain.get_manager (module RelImpl)) in
    let module AD = ApronDomain.D2 (Man) in
    let diff_box = GobConfig.get_bool "ana.apron.invariant.diff-box" in
    let module AD = (val if diff_box then (module ApronDomain.BoxProd (AD): RelationDomain.RD) else (module AD)) in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec =
    struct
      include SpecFunctor (Priv) (AD) (ApronPrecCompareUtil.Util)
      let name () = "apron"
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


let () =
  Printexc.register_printer
    (function
      | Apron.Manager.Error e ->
        let () = Apron.Manager.print_exclog Format.str_formatter e in
        Some(Printf.sprintf "Apron.Manager.Error\n %s" (Format.flush_str_formatter ()))
      | _ -> None (* for other exceptions *)
    )
