open ZExt
open Intv
open Batteries
open StringUtils

(** Provides functions and types for collections of Intv. *)
module Boxes  = 
struct
  type t = Intv.t list [@@deriving eq, ord]
  include ZExtOps
  let is_bot (i: t) = 
    BatList.exists Intv.is_bot i

  let is_top i =
    BatList.for_all Intv.is_top i

  let to_string (intervals: t) =
    let string_of_interval i intv =
      Printf.sprintf "%i->%s" i (Intv.to_string intv)
    in
    let bot_or_top =
      if is_bot intervals then
        StringUtils.bot_str
      else if is_top intervals then
        StringUtils.top_str
      else 
        " "
        |> Printf.sprintf "(%s)"
    in
    Printf.sprintf "%s { %s }" bot_or_top (String.concat "; " (List.mapi string_of_interval intervals))

  let equal boxes1 boxes2 =
    BatList.for_all2 (Intv.equal) boxes1 boxes2

  let leq i1 i2 =
    BatList.for_all2 Intv.leq i1 i2

  let join (i1: t) (i2: t) = 
    BatList.map2 Intv.union i1 i2

  let meet (i1: t) (i2: t) = 
    BatList.map2 Intv.inter i1 i2

  let widen (i1: t) (i2: t) = 
    BatList.map2 Intv.widen i1 i2

  let narrow (i1: t) (i2: t) = 
    meet i1 i2

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
          insert_dimensions (left @ [Intv.top ()] @ right) new_array
      in
      insert_dimensions intervals change_arr
  ;;

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

  let forget_vars (vars: int BatList.t) =
    BatList.mapi (fun x intv -> if BatList.mem x vars then Intv.top() else intv)

  let set_value (var: int) (value: Intv.t) = BatList.modify_at var (fun _ -> value) 

  let get_value (var:int) boxes = BatList.at boxes var
end