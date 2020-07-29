(* header for all files *)
module All = struct
  include (Batteries : module type of Batteries with module Format := Batteries.Format)
  module Format = Batteries.Legacy.Format
  let comp2 f g a b = f (g a) (g b)
  let compareBy ?cmp:(cmp=compare) f = comp2 cmp f
  let flat_map f = List.flatten % List.map f
  let str_remove m s = String.nreplace ~str:s ~sub:m ~by:""
  (* Sys.time gives runtime in seconds as float *)
  let split_time () = (* gives CPU time in h,m,s,ms *)
    let f = Sys.time () in
    let i = int_of_float f in
    let ms = int_of_float (Float.modulo f 1.0 *. 1000.) in
    i / 3600, i / 60 mod 60, i mod 60, ms
  let string_of_time () =
    let h,m,s,ms = split_time () in
    Printf.sprintf "%02d:%02d:%02d.%03d" h m s ms
end
include All (* shortcut so that 'open Prelude' is enough *)

(* header for files in analyses *)
module Ana = struct
  include All
  (* CIL *)
  include Cil
  let d_varinfo () x = d_lval () (Var x, NoOffset)
  include Pretty
  let sprint f x = Pretty.sprint 80 (f () x)
  (* Analyses.Spec etc. *)
  (* include Analyses (* circular build :( *) *)
  (* module M = Messages (* same, but this is in Analyses anyway *) *)
end
