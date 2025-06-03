(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron
open BatList

module INTERVALS  = 
struct

  type interval = Z.t * Z.t [@@deriving eq, hash, ord]
  type t = interval list [@@deriving eq, hash, ord]

  (**
     Creates a single interval from the supplied integer values.
  *)
  let create_single z1 z2 = (Z.of_int z1, Z.of_int z2)

  (**
     Sets the lowerbound of the given interval to the supplied integer value.
  *)
  let set_lb (lowerbound:int) (i: interval) = (Z.of_int lowerbound, snd i)


  (**
     Sets the upperbound of the given interval to the supplied integer value.
  *)
  let set_ub (upperbound:int) (i: interval) = (fst i, Z.of_int upperbound)

  let equal intv1 intv2 =
    let tuple_equal (a1, b1) (a2, b2) = Z.equal a1 a2 && Z.equal b1 b2 in
    BatList.for_all2 tuple_equal intv1 intv2

  let leq_single (i1: interval) (i2: interval) =
    fst i1 >= fst i2 && snd i1 <= snd i2

  let join_single (i1: interval) (i2: interval) =
    (Z.min (fst i1) (fst i2), Z.max (snd i1) (snd i2))

  let meet_single (i1: interval) (i2: interval) =
    let l = Z.max (fst i1) (fst i2) in
    let u = Z.min (snd i1) (snd i2) in
    (l, u)

  let top_single () = 
    (Z.of_int min_int, Z.of_int max_int)

  let is_top_single (i: interval) =
    fst i = Z.of_int min_int && snd i = Z.of_int max_int

  let widen_single (i1: interval) (i2: interval) =
    let l = if fst i1 <= fst i2 then fst i2 else Z.of_int min_int in
    let u = if snd i1 >= snd i2 then snd i2 else Z.of_int max_int in
    (l, u)

  let narrow_single (i1: interval) (i2: interval) = 
    meet_single i1 i2

  let is_bot_single (i: interval) =
    fst i > snd i

  let leq (i1: t) (i2: t) = 
    BatList.for_all2 leq_single i1 i2

  let join (i1: t) (i2: t) = 
    BatList.map2 join_single i1 i2

  let meet (i1: t) (i2: t) = 
    BatList.map2 meet_single i1 i2

  let top () = 
    [top_single ()]

  let is_top (i: t) = 
    BatList.for_all is_top_single i

  let widen (i1: t) (i2: t) = 
    BatList.map2 widen_single i1 i2

  let narrow (i1: t) (i2: t) = 
    meet i1 i2

  let is_bot (i: t) = 
    BatList.exists is_bot_single i

  let bot () = 
    [(Z.of_int max_int, Z.of_int min_int)]

  let sup (x: interval) = 
    snd x

  let inf (x: interval) = 
    fst x

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
          insert_dimensions (left @ top() @ right) new_array
      in
      insert_dimensions intervals change_arr

  let dim_remove (dim_change: Apron.Dim.change) (intervals : t) =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      List.filteri (fun i _ -> not (Array.mem i dim_change.dim)) intervals

  (*
  TODO: Evaluate if the dim_change.dim is always sorted.
    *)
  (* precondition: dim_change is sorted and has unique elements *)
  (* let dim_remove_on_sorted_dim_change (dim_change: Apron.Dim.change) (intervals : t) =
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
      aux 0 0 intervals *)

  let to_string (intervals: t) =
    if is_bot intervals then
      "bot"
    else if is_top intervals then
      "top"
    else
      let string_of_interval (l, u) =
        Printf.sprintf "[%s, %s]" (Z.to_string l) (Z.to_string u)
      in
      "{" ^ (String.concat "; " (List.map string_of_interval intervals)) ^ "}"
end

module SUB =
struct
  module Idx = Int
  module VarSet = BatSet.Make(Idx)
  module VarList = BatList

  module MoveMap = struct 
    include BatMap.Make(Idx)
    type t = Idx.t BatMap.Make(Idx).t
  end


  type t = VarSet.t VarList.t [@@deriving eq, ord]


  let dim_add (dim_change: Apron.Dim.change) (sub: t) =
    if dim_change.realdim != 0 then 
      failwith "Pentagons are defined over integers: \
                dim_change should not contain `realdim`" 
    else
      (* 
      This is basically a fold_lefti with rev at the end.
      Could not use fold_lefti directly because I might need to append at the end of the list.
      This would have forced me to use List.length, which is \theta(n).
      *)
      let rec aux (dim_change: Apron.Dim.change) i sub (moved: MoveMap.t) acc =
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
        match sub with
        | h::t ->
          (** Store the new index mappings to later adjust the sets. *)
          let moved = (MoveMap.add i (i+moved_by) moved) in
          (** Insert `append_count` many dimensions before `h`, then append `h` *)
          let acc = (h :: (prepend_dim append_count acc)) in
          aux dim_change (i+1) t moved acc
        | [] ->
          (** Complete sub prepending the last dimensions and reversing *)
          let sub = (List.rev (prepend_dim append_count acc)) in
          (** Adjust the stored indices in our sets *)
          VarList.map (
            fun set ->
              VarSet.map (
                fun v -> match MoveMap.find_opt v moved with | None -> v | Some(v') -> v'
              ) set
          ) sub 
      in
      aux dim_change 0 sub MoveMap.empty []
  ;;


  let dim_remove (dim_change: Apron.Dim.change) (sub : t) =
    (* This implementation assumes, that dim_change.dim is well-formed, i.e., does not contain duplicates. *)
    let move_or_delete_var y =
      if Array.mem y dim_change.dim then None
      else Some (y - Array.count_matching (fun k -> k < y) dim_change.dim)
    in
    let move_or_delete_set x ys =
      if Array.mem x dim_change.dim then None
      else Some (VarSet.filter_map move_or_delete_var ys)
    in
    List.filteri_map move_or_delete_set sub

  let equal (sub1: t) (sub2: t) = VarList.equal VarSet.equal sub1 sub2

  let bot () = failwith "TODO" (* empty list? *)

  (**
        This isn't precise: we might return false even if there are transitive contradictions;
        Other possibility: compute transitive closure first (would be expensive)
  *)
  let is_bot (sub:t) =
    (* exists function for lists where the predicate f also gets the index of a list element *)
    let existsi f lst =
      let rec aux i = function
        | [] -> false
        | x :: xs -> if f i x then true else aux (i + 1) xs
      in aux 0 lst
    in
    sub = [] || (* if we don't know any variables, bot = top *)
    existsi (fun x ys -> VarSet.mem x ys ||
                         VarSet.exists (fun y -> VarSet.mem x (List.nth sub y)) ys) sub

  let top () = failwith "TODO" (* empty list? *)

  let is_top (sub: t) = VarList.for_all VarSet.is_empty sub

  let subseteq set1 set2 = VarSet.subset set1 set2 || VarSet.equal set1 set2 (** helper, missing in batteries *)

  (**
     The inequalities map s1 is less than or equal to s2 iff
      forall x in s2.
      s2(x) subseteq s1(x)
  *)
  let leq (sub1: t) (sub2: t) = BatList.for_all2 subseteq sub2 sub1

  let join (sub1: t) (sub2: t) = BatList.map2 VarSet.inter sub1 sub2

  let meet (sub1: t) (sub2: t) = BatList.map2 VarSet.union sub1 sub2

  let widen (sub1: t) (sub2: t) = BatList.map2 (fun s1x s2x -> if subseteq s1x s2x then s2x else VarSet.empty) sub1 sub2

  (** No narrowing mentioned in the paper. *)
  let narrow sub1 sub2 = meet sub1 sub2

  let to_string (sub: t) =
    (* Results in: { y1, y2, ..., yn }*)
    let set_string set = "{" ^ (
        VarSet.to_list set |>
        List.map (Idx.to_string) |>
        String.concat ","
      ) ^ "}" in
    (* Results in: x_1 -> {y1, y2, ..., yn} *)
    let relations_string = String.concat ", " (VarList.mapi (
        fun  i set ->
          (Idx.to_string i) ^ " -> " ^ (set_string set)
      ) sub) in
    (* Results in:
        {
        x_1 -> {y1, y2, ..., yn}
        }
    *)
    "{" ^ relations_string ^ "}"

  let to_string (sub: t) = 
    (* if is_bot sub then
       "bot"
       else if is_top sub then
       "top"
       else *)
    to_string sub 

end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module PNTG =
struct
  type t = { intv: INTERVALS.t; sub: SUB.t } [@@deriving eq, ord]

  let hash : (t -> int)  = fun _ -> failwith "TODO"
  let equal _ _ = failwith "TODO"
  let compare _ _ = failwith "TODO"
  let copy (x: t) = x
  let empty () = failwith "TODO"

  let is_empty _ = failwith "TODO"

  (**
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change
  *)
  let dim_add (dim_change: Apron.Dim.change) pntg =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let intv, sub = 
        INTERVALS.dim_add dim_change pntg.intv,
        SUB.dim_add dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)

  (** 
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change 
  *)
  let dim_remove (dim_change: Apron.Dim.change) pntg  =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let intv, sub = 
        INTERVALS.dim_remove dim_change pntg.intv,
        SUB.dim_remove dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)
end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  module PNTG = PNTG
  include SharedFunctions.VarManagementOps (PNTG)
end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr = failwith "TODO"

  let bound_texpr d texpr1 = Timing.wrap "bounds calculation" (bound_texpr d) texpr1
end


module D =
struct
  include Printable.Std
  include VarManagement
  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)
  (**
     TODO: module Tracked
  *)
  module Tracked = struct let varinfo_tracked _ = failwith "TODO Tracked";; let type_tracked _ = failwith "TODO Tracked";; end

  type var = V.t

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

  (**
     Bottom creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: bot = top.
  *)
  let bot () = failwith "TODO" (* { intv = INTERVALS.bot (); sub = SUB.bot () } *)

  (**
     Top creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: top = bot.
  *)
  let top () = failwith "TODO" (* { intv = INTERVALS.top (); sub = SUB.top () } *)

  let is_bot t = failwith "TODO" (* INTERVALS.is_bot t.intv || SUB.is_bot t.sub *)

  let is_top t = failwith "TODO" (* INTERVALS.is_top t.intv && SUB.is_top t.sub *)

  let show varM = failwith "TODO"
  let pretty () (x:t) = failwith "TODO"
  let printXml f x = failwith "TODO"

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {intv = INTERVALS.meet d1'.intv d2'.intv; sub = SUB.meet d1'.sub d2'.sub}; env = sup_env}: t)
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 = 
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Timing.wrap "meet" (meet t1) t2

  let leq t1 t2 = 
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let interval1, interval2 = d1'.intv, d2'.intv in
      let sub1, sub2 = d1'.sub, d2'.sub in
      let for_alli f lst =
        List.for_all (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) lst) in
      let bool1 = INTERVALS.leq interval1 interval2 in
      let bool2 = for_alli(fun i s2x -> 
          SUB.VarSet.for_all(fun y -> 
              let s1x = SUB.VarList.at sub1 i in
              let b1x = BatList.at interval1 i in
              let b1y = BatList.at interval1 y in
              SUB.VarSet.exists (Int.equal y) s1x ||
              INTERVALS.sup b1x < INTERVALS.inf b1y
            ) s2x
        ) sub2 in
      bool1 && bool2
    | _ -> false

  let leq a b = Timing.wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let intv_join = INTERVALS.join d1'.intv d1'.intv in
      let s' x s1x = SUB.VarSet.inter s1x (List.nth d2'.sub x) in
      let s'' x s1x = SUB.VarSet.filter (fun y -> INTERVALS.sup (List.nth d2'.intv x) < INTERVALS.inf (List.nth d2'.intv y)) s1x in
      let s''' x = SUB.VarSet.filter (fun y -> INTERVALS.sup (List.nth d1'.intv x) < INTERVALS.inf (List.nth d1'.intv y)) (List.nth d2'.sub x) in
      let sub_join = List.mapi (fun x s1x -> SUB.VarSet.union (s' x s1x) (SUB.VarSet.union (s'' x s1x) (s''' x))) d1'.sub in

      ({d = Some {intv = intv_join; sub = sub_join}; env = sup_env}: t)
    | Some d1', None -> {d = Some d1'; env = sup_env}
    | None, Some d2' -> {d = Some d2'; env = sup_env}
    | _ -> {d = None; env = sup_env}

  let join a b = Timing.wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let widen t1 t2 = 
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {intv = INTERVALS.widen d1'.intv d2'.intv; sub = SUB.widen d1'.sub d2'.sub}; env = sup_env}: t)
    | _ -> {d = None; env = sup_env}

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow t1 t2 = meet t1 t2

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) = failwith "TODO pretty_diff"


  (* S2 Specific functions of RelationDomain *)
  let is_bot_env t = t.d = None

  let forget_vars _ = failwith "TODO forget_vars"

  let assign_exp _ = failwith "TODO assign_exp"
  let assign_var _ = failwith "TODO assign_var"

  let assign_var_parallel_with _ = failwith "TODO assign_var_parallel_with"

  let assign_var_parallel' _ = failwith "TODO assign_var_parallel"
  let substitute_exp _ = failwith "TODO substitute_exp"
  let unify pntg1 pntg2 = meet pntg1 pntg2

  type marshal = t
  let marshal t = t
  let unmarshal t = t

  let assert_inv _ = failwith "SF but we most likely need assert_constraint"
  let invariant _ = failwith "TODO invariant"
  let equal _ = failwith "TODO equal"
  let hash _ = failwith "TODO hash"
  let compare _ = failwith "TODO compare"
  let relift _ = failwith "TODO relift"
  let eval_int _ = failwith "TODO eval_int"

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

end

