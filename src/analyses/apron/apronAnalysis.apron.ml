open Analyses

module ExtendedSpecFunctor (CPriv: RelationPriv.S) (RD: RelationDomain.RD) : Analyses.MCPSpec =
struct
  module OctApron = ApronPrecCompareUtil.OctagonD
  include  RelationAnalysis.SpecFunctor (CPriv) (RD) (ApronPrecCompareUtil.Util)
  module AD = ApronDomain.D2Complete(OctApron.Man)
  module PCU = ApronPrecCompareUtil.Util(OctApron)

  let results = PCU.RH.create 103 (*ToDo This should not be created again!*)

  let init marshal =
    Priv.init ()

  let name () = "apron"

  let store_data file =
    let convert (m: AD.t PCU.RH.t): OctApron.t PCU.RH.t =
      let convert_single (a: AD.t): OctApron.t =
        if Oct.manager_is_oct AD.Man.mgr then
          Oct.Abstract1.to_oct a
        else
          let generator = AD.to_lincons_array a in
          OctApron.of_lincons_array generator
      in
      PCU.RH.map (fun _ -> convert_single) m
    in
    let post_process m =
      let m = Stats.time "convert" convert m in
      PCU.RH.map (fun _ v -> OctApron.marshal v) m
    in
    let results = post_process results in
    let name = name () ^ "(domain: " ^ (AD.Man.name ()) ^ ", privatization: " ^ (Priv.name ()) ^ (if GobConfig.get_bool "ana.apron.threshold_widening" then ", th" else "" ) ^ ")" in
    let results: ApronPrecCompareUtil.dump = {marshalled = results; name } in
    Serialize.marshal results file

    let finalize () =
      let file = GobConfig.get_string "exp.apron.prec-dump" in
      if file <> "" then begin
        Printf.printf "exp.apron.prec-dump is potentially costly (for domains other than octagons), do not use for performance data!\n";
        Stats.time "apron.prec-dump" store_data (Fpath.v file)
      end;
      Priv.finalize ()
end

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module Man = (val ApronDomain.get_manager ()) in
    let module AD = ApronDomain.D2 (Man) in
    let module RD: RelationDomain.RD =
    struct
      module Var = SharedFunctions.Var
      module V = RelationDomain.V(Var)
      include AD
    end in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec = ExtendedSpecFunctor (Priv) (RD) in
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
