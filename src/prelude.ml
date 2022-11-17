(* header for all files *)
module All = struct
  include (Batteries : module type of Batteries with module Format := Batteries.Format)
  module Format = Batteries.Legacy.Format
end
include All (* shortcut so that 'open Prelude' is enough *)

(* header for files in analyses *)
module Ana = struct
  include All
  (* CIL *)
  include GoblintCil
  let d_varinfo () x = d_lval () (Var x, NoOffset)
  include Pretty
  let sprint f x = Pretty.sprint ~width:max_int (f () x)
  (* Analyses.Spec etc. *)
  (* include Analyses (* circular build :( *) *)
  (* module M = Messages (* same, but this is in Analyses anyway *) *)
end
