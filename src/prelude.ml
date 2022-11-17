(* header for all files *)
module All = struct
  include (Batteries : module type of Batteries with module Format := Batteries.Format)
  module Format = Batteries.Legacy.Format

  (* Sys.time gives runtime in seconds as float *)
  let split_time () = (* gives CPU time in h,m,s,ms *)
    let f = Sys.time () in
    let i = int_of_float f in
    let ms = int_of_float (Float.modulo f 1.0 *. 1000.) in
    i / 3600, i / 60 mod 60, i mod 60, ms
  let string_of_time () = (* CPU time as hh:mm:ss.ms *)
    let h,m,s,ms = split_time () in
    Printf.sprintf "%02d:%02d:%02d.%03d" h m s ms

  let localtime () =
    let open Unix in
    let tm = time () |> localtime in
    Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
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
