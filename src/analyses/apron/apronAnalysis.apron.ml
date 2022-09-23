(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses

include RelationAnalysis

module ExtendedSpecFunctor (AD: RelationDomain.RD) (Priv: RelationPriv.S) =
struct

  module OctApron = ApronPrecCompareUtil.OctagonD
  include SpecFunctor (Priv) (AD) (ApronPrecCompareUtil.Util)
  module AD = ApronDomain.D2(OctApron.Man)
  module PCU = ApronPrecCompareUtil.Util(OctApron)

  let results = PCU.RH.create 103 (*ToDo This should not be created again!*)

  open AD
  open (ApronDomain: (sig module V: (module type of ApronDomain.V) end)) (* open only V from ApronDomain (to shadow V of Spec), but don't open D (to not shadow D here) *)

  open ApronPrecCompareUtil

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
    let name = name () ^ "(domain: " ^ (AD.name ()) ^ ", privatization: " ^ (Priv.name ()) ^ (if GobConfig.get_bool "ana.apron.threshold_widening" then ", th" else "" ) ^ ")" in
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
    let module AD = ApronDomain.D3 (Man) in
    let diff_box = GobConfig.get_bool "ana.apron.invariant.diff-box" in
    let module AD = (val if diff_box then (module ApronDomain.BoxProd (AD): ApronDomain.S3) else (module AD)) in
    let module RD: RelationDomain.RD =
    struct
      module Var = ApronDomain.Var
      module V = ApronDomain.V
      include AD
      type var = ApronDomain.Var.t
      type consSet = ApronDomain.Lincons1Set.elt
    end in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec = ExtendedSpecFunctor (RD) (Priv) in
    (module Spec)
  )

let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let after_config () =
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

  let () =
  Printexc.register_printer
    (function
      | Apron.Manager.Error e ->
        let () = Apron.Manager.print_exclog Format.str_formatter e in
        Some(Printf.sprintf "Apron.Manager.Error\n %s" (Format.flush_str_formatter ()))
      | _ -> None (* for other exceptions *)
    )

let _ =
  AfterConfig.register after_config
