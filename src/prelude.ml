(* header for all files *)
module All = struct
  include (Batteries : module type of Batteries with module Format := Batteries.Format)
  module Format = Batteries.Legacy.Format
  let comp2 f g a b = f (g a) (g b)
  let compareBy ?cmp:(cmp=compare) f = comp2 cmp f
  let str_remove m s = String.nreplace ~str:s ~sub:m ~by:""

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
  let d_varinfo ppf x = d_lval ppf (Var x, NoOffset)
  include Pretty
  let sprint f x = Fmt.str "%a" f x
  (* Analyses.Spec etc. *)
  (* include Analyses (* circular build :( *) *)
  (* module M = Messages (* same, but this is in Analyses anyway *) *)
end

let () =
  let width = max_int in
  let ppf = Format.str_formatter in
  (* Format.pp_set_geometry Format.str_formatter ~margin:width ~max_indent:(width - 1); *)
  (* TODO: workaround for https://github.com/ocaml/ocaml/issues/11517 *)
  Format.pp_set_margin ppf width;
  Format.pp_set_max_indent ppf (Format.pp_get_margin ppf () - 1);
