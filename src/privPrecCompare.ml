open! Defaults (* CircInterval / Enums / ... need initialized conf *)
open! Batteries
open Prelude
open Ana

module VD = BaseDomain.VD
(* TODO: share with PrecisionDumpPriv *)
module LVH =
struct
  include Hashtbl.Make (Printable.Prod (Basetype.ProgLines) (Basetype.Variables))

  let iter2 f h1 h2 =
    ignore (
      merge (fun k v1 v2 ->
          f k v1 v2;
          None
        ) h1 h2
    )
end

let load filename =
  let f = open_in_bin filename in
  let lvh: VD.t LVH.t = Marshal.from_channel f in
  close_in_noerr f;
  lvh

let compare_dumps filename1 filename2 =
  let lvh1 = load filename1 in
  let lvh2 = load filename2 in
  LVH.iter2 (fun (l, x) v1 v2 ->
      let v1 = v1 |? VD.bot () in
      let v2 = v2 |? VD.bot () in
      let diff_msg =
        match VD.leq v1 v2, VD.leq v2 v1 with
        | true, true -> None
        | true, false -> Some (Pretty.dprintf "%s lt %s (%a lt %a)" filename1 filename2 VD.pretty v1 VD.pretty v2)
        | false, true -> Some (Pretty.dprintf "%s gt %s (%a gt %a)" filename1 filename2 VD.pretty v1 VD.pretty v2)
        | false, false -> Some (Pretty.dprintf "%s incmp %s (%a incmp %a)" filename1 filename2 VD.pretty v1 VD.pretty v2)
      in
      match diff_msg with
      | Some diff_msg ->
        ignore (Pretty.printf "%a %a: %t\n" d_loc l d_varinfo x (fun () -> diff_msg))
      | None -> ()
    ) lvh1 lvh2

let () =
  let filenames = List.tl (Array.to_list Sys.argv) in
  List.cartesian_product filenames filenames
  |> List.filter (fun (filename1, filename2) -> String.compare filename1 filename2 < 0)
  |> List.iter (uncurry compare_dumps)