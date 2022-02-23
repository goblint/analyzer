(** Ref: Affine Relationships Among Variables of a Program, Michael Karr 1976
    https://link.springer.com/content/pdf/10.1007/BF00268497.pdf *)

open Prelude.Ana
open Analyses

open RelationAnalysis

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module AD = AffineEqualityDomain.AD2 in
    let module RD: RelationDomain.RD =
    struct
      module Var = SharedFunctions.Var
      module V = RelationDomain.V(Var)
      module D2 = AD
    end in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec = SpecFunctor (Priv) (RD) (RelationPrecCompareUtil.DummyUtil) in
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
