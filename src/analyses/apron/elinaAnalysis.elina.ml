(** {{!RelationAnalysis} Relational integer value analysis} using {!Elina} domains ([elina]). *)

open Analyses
open RelationalAnalysis
open RelationalImplementation
open ElinaImplementation

include RelationAnalysis

let after_config () =
  RelationalAnalysis.after_config (module ElinaImplementation) "elina"

let _ =
  AfterConfig.register after_config

let () =
  RelationalAnalysis.register
