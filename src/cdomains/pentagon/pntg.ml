open StringUtils
open GobApron
module M = Messages

open Boxes
open Subs


module Pntg =
struct
  type t = { boxes: Boxes.t; subs: Subs.t } [@@deriving eq, ord, hash]

  let copy (x: t) = x
  let empty () = { boxes = []; subs = [] }
  let is_empty pntg =
    match pntg.boxes, pntg.subs with
    | [], [] -> true
    | _ -> false

  let to_string pntg =
    Printf.sprintf "{ boxes = %s; subs = %s }" (Boxes.to_string pntg.boxes) (Subs.to_string pntg.subs)


  (**
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change
  *)
  let dim_add (dim_change: Apron.Dim.change) pntg =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else
      let intv, subs = 
        Boxes.dim_add dim_change pntg.boxes,
        Subs.dim_add dim_change pntg.subs 
      in
      ({boxes = intv; subs = subs}: t)

  let dim_add (dim_change: Apron.Dim.change) pntg = 
    let res = dim_add dim_change pntg in
    if M.tracing then M.trace "pntg_dim" "PNTG.dim_add:\ndim_change:\t%s\npntg:\t\t%s\nres:\t\t%s\n\n" 
        (StringUtils.string_of_dim_change dim_change)
        (to_string pntg)
        (to_string res);
    res

  let dim_add dim_change pntg = Timing.wrap "dim_add" (dim_add dim_change) pntg

  (** 
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change 
  *)
  let dim_remove (dim_change: Apron.Dim.change) pntg  =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else
      let boxes, subs = 
        Boxes.dim_remove dim_change pntg.boxes,
        Subs.dim_remove dim_change pntg.subs 
      in
      ({boxes = boxes; subs = subs}: t)

  let dim_remove (dim_change: Apron.Dim.change) pntg = 
    let res = dim_remove dim_change pntg in
    if M.tracing then M.trace "pntg_dim" "PNTG.dim_remove:\ndim_change:\t%s\npntg:\t\t%s\nres:\t\t%s\n\n" 
        (StringUtils.string_of_dim_change dim_change)
        (to_string pntg)
        (to_string res);
    res


  let dim_remove dim_change pntg = Timing.wrap "dim_remove" (dim_remove dim_change) pntg
end