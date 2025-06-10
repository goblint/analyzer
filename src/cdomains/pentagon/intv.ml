open ZExt
open Batteries
open PentaInterval
module Interval = PentaInterval.Interval

(** Provides functions and types for collections of Interval. *)
module Intv  = 
struct
  type t = Interval.t list [@@deriving eq, ord]

  let equal intv1 intv2 =
    BatList.for_all2 (Interval.equal) intv1 intv2

  let leq i1 i2 =
    BatList.for_all2 Interval.leq i1 i2

  let join (i1: t) (i2: t) = 
    BatList.map2 Interval.union i1 i2

  let meet (i1: t) (i2: t) = 
    BatList.map2 Interval.inter i1 i2

  let is_top i =
    BatList.for_all Interval.is_top i

  let widen (i1: t) (i2: t) = 
    BatList.map2 Interval.widen i1 i2

  let narrow (i1: t) (i2: t) = 
    meet i1 i2

  let is_bot (i: t) = 
    BatList.exists Interval.is_bot i


  let dim_add (dim_change: Apron.Dim.change) (intervals: t) =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let change_arr = Array.rev dim_change.dim in
      let rec insert_dimensions intervals dim_changes =
        match dim_changes with
        | [||] -> intervals
        | _ ->
          let k = dim_changes.(0) in
          let left, right = BatList.split_at k intervals in
          let new_array = (BatArray.sub dim_changes 1 (BatArray.length dim_changes - 1)) in
          insert_dimensions (left @ [Interval.top ()] @ right) new_array
      in
      insert_dimensions intervals change_arr;;


  (* Backup implementation, if dim_change.dim is not sorted and contains duplicates. *)
  (*let dim_remove (dim_change: Apron.Dim.change) (intervals : t) =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      List.filteri (fun i _ -> not (Array.mem i dim_change.dim)) intervals*)


  (* precondition: dim_change is sorted and has unique elements *)
  let dim_remove (dim_change: Apron.Dim.change) (intervals : t) =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else
      let rec aux lst_i arr_i = function
        | [] -> []
        | x::xs -> if arr_i = BatArray.length dim_change.dim then x::xs
          else if dim_change.dim.(arr_i) = lst_i then aux (lst_i + 1) (arr_i + 1) xs
          else x :: aux (lst_i + 1) arr_i xs
      in
      aux 0 0 intervals

  let to_string (intervals: t) =
    if is_bot intervals then
      "bot"
    else if is_top intervals then
      "top"
    else
      let string_of_interval (l, u) =
        Printf.sprintf "[%s, %s]" (ZExt.to_string l) (ZExt.to_string u)
      in
      "{" ^ (String.concat "; " (List.map string_of_interval intervals)) ^ "}"


  let forget_vars (vars: int BatList.t) =
    BatList.mapi (fun x intv -> if BatList.mem x vars then Interval.top() else intv)

end