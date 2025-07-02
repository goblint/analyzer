open Batteries
open GobApron

open StringUtils

(** Stores functions and types for strict upper bounds. *)
module Subs =
struct

  module Idx = Int
  module VarSet = BatSet.Make(Idx)
  module VarList = BatList

  module MoveMap = struct 
    include BatMap.Make(Idx)
    type t = Idx.t BatMap.Make(Idx).t
  end

  type t = VarSet.t VarList.t [@@deriving eq, ord]

  let dim_add (dim_change: Apron.Dim.change) (subs: t) =
    if dim_change.realdim != 0 then 
      failwith "Pentagons are defined over integers: \
                dim_change should not contain `realdim`" 
    else
      (* 
      This is basically a fold_lefti with rev at the end.
      Could not use fold_lefti directly because I might need to append at the end of the list.
      This would have forced me to use List.length, which is \theta(n).
      *)
      let rec aux (dim_change: Apron.Dim.change) i subs (moved: MoveMap.t) acc =
        (** Computes the number by which the current index/variable will be shifted/moved *)
        let moved_by = 
          Array.count_matching (fun k -> k <= i) dim_change.dim 
        in
        (** Counts the number of empty sets to prepend at the current index. *)
        let append_count = 
          Array.count_matching (fun k -> k == i) dim_change.dim 
        in
        (** Prepends n many empty sets to the accumulator. *)
        let rec prepend_dim n acc = 
          if n == 0 then 
            acc
          else prepend_dim (n-1) (VarSet.empty :: acc)
        in
        match subs with
        | h::t ->
          (** Store the new index mappings to later adjust the sets. *)
          let moved = (MoveMap.add i (i+moved_by) moved) in
          (** Insert `append_count` many dimensions before `h`, then append `h` *)
          let acc = (h :: (prepend_dim append_count acc)) in
          aux dim_change (i+1) t moved acc
        | [] ->
          (** Complete subs prepending the last dimensions and reversing *)
          let subs = (List.rev (prepend_dim append_count acc)) in
          (** Adjust the stored indices in our sets *)
          VarList.map (
            fun set ->
              VarSet.map (
                fun v -> match MoveMap.find_opt v moved with | None -> v | Some(v') -> v'
              ) set
          ) subs 
      in
      aux dim_change 0 subs MoveMap.empty []
  ;;


  let dim_remove (dim_change: Apron.Dim.change) (subs: t) =
    (* This implementation assumes, that dim_change.dim is well-formed, i.e., does not contain duplicates. *)
    let move_or_delete_var y =
      if Array.mem y dim_change.dim then None
      else Some (y - Array.count_matching (fun k -> k < y) dim_change.dim)
    in
    let move_or_delete_set x ys =
      if Array.mem x dim_change.dim then None
      else Some (VarSet.filter_map move_or_delete_var ys)
    in
    List.filteri_map move_or_delete_set subs

  let equal (sub1: t) (sub2: t) = VarList.equal VarSet.equal sub1 sub2

  (**
        This isn't precise: we might return false even if there are transitive contradictions;
        Other possibility: compute transitive closure first (would be expensive)
  *)
  let is_bot (subs: t) =
    (* exists function for lists where the predicate f also gets the index of a list element *)
    let existsi f lst =
      let rec aux i = function
        | [] -> false
        | x :: xs -> if f i x then true else aux (i + 1) xs
      in aux 0 lst
    in
    (* If we don't know any variables, i.e. subs = [], then bot = top holds. *)
    subs = [] || 
    existsi (
      fun i set ->
        (* direct contradiction *)
        VarSet.mem i set ||
        (* We assume that the sets inside subs only contain values < length of the list.*)
        VarSet.exists (fun y -> VarSet.mem i (List.at subs y)) set
    ) subs


  let is_top (sub: t) = VarList.for_all VarSet.is_empty sub

  let subseteq set1 set2 = VarSet.subset set1 set2 || VarSet.equal set1 set2 (** helper, missing in batteries *)

  (**
     The inequalities map s1 is less than or equal to s2 iff
      forall x in s2.
      s2(x) subseteq s1(x)
  *)
  let leq (sub1: t) (sub2: t) = 
    BatList.for_all2 subseteq sub2 sub1

  let join (sub1: t) (sub2: t) = BatList.map2 VarSet.inter sub1 sub2

  let meet (sub1: t) (sub2: t) = BatList.map2 VarSet.union sub1 sub2

  let widen (sub1: t) (sub2: t) = BatList.map2 (fun s1x s2x -> if subseteq s1x s2x then s2x else VarSet.empty) sub1 sub2

  (** No narrowing mentioned in the paper. *)
  let narrow sub1 sub2 = meet sub1 sub2

  let forget_vars (vars : int BatList.t) =
    BatList.mapi (fun x ys ->
        if BatList.mem x vars then
          VarSet.empty
        else
          VarSet.filter (fun y -> not (BatList.mem y vars)) ys
      )

  let set_value (index: Idx.t) (value: VarSet.t) = BatList.modify_at index (fun _ -> value)

  let get_value (index: Idx.t) (subs: t) = BatList.at subs index

  let to_string (sub: t) =
    (* The following results in: { y1#, y2#, ..., yn# }*)
    let set_string set = 
      if VarSet.cardinal set = 0 then "∅" else "{" ^ (
          VarSet.to_list set |>
          (* Marking for later variable name replacement *)
          List.map (fun v -> Idx.to_string v ^ "#") |> 
          String.concat ", "
        ) ^ "}" in
    (* The following results in: x_1 -> {y1#, y2#, ..., yn#}; ... ; x_n -> {y1#, ..., yn#} *)
    String.concat "; " (
      VarList.mapi (
        fun i set ->
          Printf.sprintf "%s->%s" (Idx.to_string i) (set_string set)
      ) sub
    )

  let to_string (subs: t) =
    let bot_or_top = 
      if is_bot subs then
        StringUtils.bot_str
      else if is_top subs then
        StringUtils.top_str
      else
        " "
        |> Printf.sprintf "(%s)"
    in
    Printf.sprintf "%s { %s }" bot_or_top (to_string subs)

  (* x_i < x_j <== x_j \in SUBs(x_i) *)
  let lt subs i j =
    let subs_i = List.at subs i in
    VarSet.mem j subs_i

  let gt subs i j =
    lt subs j i

end