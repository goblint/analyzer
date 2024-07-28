(** {{!RelationAnalysis} Relational integer value analysis} using {!Apron} domains ([apron]). *)

open Analyses
open RelationalAnalysis
open RelationalImplementation
open ApronImplementation

include RelationAnalysis

let after_config () =
  RelationalAnalysis.after_config (module ApronImplementation) "apron"

let _ =
  AfterConfig.register after_config

let () =
  RelationalAnalysis.register
