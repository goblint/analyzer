(* header for all files *)
module All = struct
  include (Batteries : module type of Batteries with module Format := Batteries.Format)
  module Format = Batteries.Legacy.Format
  let comp2 f g a b = f (g a) (g b)
  let compareBy ?cmp:(cmp=compare) f = comp2 cmp f
  let flat_map f = List.flatten % List.map f
  let str_remove m s = String.nreplace ~str:s ~sub:m ~by:""
end
include All (* shortcut so that 'open Prelude' is enough *)

(* header for files in analyses *)
module Ana = struct
  include All
  (* CIL *)
  include Cil
  include Pretty
  let sprint f x = Pretty.sprint 80 (f () x)
  (* Analyses.Spec etc. *)
  (* include Analyses (* circular build :( *) *)
  (* module M = Messages (* same, but this is in Analyses anyway *) *)
end
