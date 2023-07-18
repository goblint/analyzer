open Analyses

(* TODO: how to extend special with string functions? *)
module RelationalSubstringAnalysis =
struct
  include RelationAnalysis

  let spec_module: (module MCPSpec) Lazy.t =
    lazy (
      let module Sub = RelationalStringDomains.RelationalSubstring in
      let module Priv = (val RelationPriv.get_priv ()) in
      let module Spec =
      struct
        include SpecFunctor (Priv) (Sub) (RelationPrecCompareUtil.DummyUtil)
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
end
