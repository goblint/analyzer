open Analyses

module RSD = RelationalStringDomains.RelationalSubstring
module Priv = (val RelationPriv.get_priv ())
module PCU = RelationPrecCompareUtil.DummyUtil

module RA = RelationAnalysis.SpecFunctor (Priv) (RSD) (PCU)

module RelationalSubstringAnalysis =
struct
  include RelationAnalysis

  let spec_module: (module MCPSpec) Lazy.t =
    lazy (
      let module Spec =
      struct
        include SpecFunctor (Priv) (RSD) (PCU)
        let name () = "relational_substr"    
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

  let special ctx r f args =
    let ask = Analyses.ask_of_ctx ctx in
    let st = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Strcpy { dest; src; n }, _ -> failwith "TODO" (* TODO: how to exp -> varinfo? *)
    (* RSD.string_copy ctx st dest src n *)
    | Strcat { dest; src; n }, _ -> failwith "TODO"
    | Strlen s, _ -> failwith "TODO"
    | Strstr { haystack; needle }, _ -> failwith "TODO"
    | Strcmp { s1; s2; n }, _ -> failwith "TODO"
    | _, _ -> RA.special ctx r f args
end
