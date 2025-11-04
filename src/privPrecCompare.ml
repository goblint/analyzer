open Goblint_lib
(* Utility program to compare precision of results of the base analysis with different privatizations *)
(* The result files that can be compared here may be created by passing an output file name in "exp.priv-prec-dump" to Goblint *)
module A = PrecCompare.MakeDump (PrivPrecCompareUtil)

let () =
  AnalysisState.bot_in_blob_leq_bot := true;
  A.main ()
