(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
module M = Messages
open GobApron
open BatList


(** 
   Extension of the Zarith types and funcitons.
   The values represent arbitrary precision integers and also negative or positive infinity.
*)
module ZExt =
struct
  type t = PosInfty | NegInfty | Arb of Z.t

  let hash (z: t) = 
    match z with
    | PosInfty -> failwith "TODO" 
    | NegInfty -> failwith "TODO"
    | Arb(z) -> Z.hash z;;

  let equal (z1: t) (z2: t) = 
    match z1, z2 with
    | PosInfty, PosInfty -> true
    | NegInfty, NegInfty -> true
    | Arb(z1), Arb(z2) -> Z.equal z1 z2
    | _ -> false ;;

  let compare (z1: t) (z2: t) = 
    match z1, z2 with
    | NegInfty, NegInfty -> 0
    | PosInfty, PosInfty -> 0
    | NegInfty, _ -> -1
    | _, NegInfty -> 1
    | PosInfty, _ -> 1
    | _, PosInfty -> -1
    | Arb(z1), Arb(z2) -> Z.compare z1 z2;;


  let of_int i = Arb(Z.of_int i)

  let of_float i = Arb(Z.of_float i)

  let zero = of_int 0

  let to_string = function
    | NegInfty -> "-∞"
    | PosInfty -> "+∞"
    | Arb z -> Z.to_string z

  let neg = function
    | NegInfty -> PosInfty
    | PosInfty -> NegInfty
    | Arb z -> Arb(Z.neg z)

  let sign = function
    | NegInfty -> -1
    | PosInfty -> +1
    | Arb z -> Z.sign z

  let add_opt z1 z2 =
    match z1, z2 with
    | PosInfty, NegInfty -> None
    | NegInfty, PosInfty -> None
    | Arb z1, Arb z2 -> Some(Arb(Z.add z1 z2))
    | PosInfty, _ -> Some(PosInfty)
    | NegInfty, _ -> Some(NegInfty)
    | _, PosInfty -> Some(PosInfty)
    | _, NegInfty -> Some(NegInfty)

  let add_unsafe z1 z2 =
    match add_opt z1 z2 with
    | None -> failwith "Cannot add PosInfty and NegInfty or vice versa."
    | Some(s) -> s

  let rec mul z1 z2 =
    match z1, z2 with
    | Arb z1, Arb z2 -> Arb(Z.mul z1 z2)
    | Arb(z1), z2 -> mul z2 (Arb z1)
    (** z1 is definitely a infty *)
    | z1, z2 ->
      if sign z2 < 0 then
        neg z1
      else
      if z2 = zero then
        zero
      else
        z1

  let rec div z1 z2 =
    match z1, z2 with
    | Arb z1, Arb z2 -> Arb(Z.div z1 z2)
    | Arb(z1), z2 -> div z2 (Arb z1)
    (** z1 is definitely a infty *)
    | z1, z2 ->
      if sign z2 < 0 then
        neg z1
      else
      if z2 = zero then
        zero
      else
        z1

  let abs z1 = if z1 < zero then neg z1 else z1;;

  let max z1 z2 = if z1 > z2 then z1 else z2;;

  let min z1 z2 = if z1 < z2 then z1 else z2;;

  (* let min_of_list zs = function 
     | [] -> failwith "min is undefined on empty lists!"
     (** Everything is less or equal to PosInfty. *)
     | zs -> List.fold (fun acc x -> min acc x) PosInfty zs *)

  (* let max_of_list zs = function 
     | [] -> failwith "max is undefined on empty lists!"
     (** Everything is greater or equal to NegInfty. *)
     | zs -> List.fold (fun acc x -> max acc x) NegInfty zs *)

  (** Taken from module IArith *)
  let min4 a b c d = min (min a b) (min c d)

  (** Taken from module IArith *)
  let max4 a b c d = max (max a b) (max c d)

end

(**
   Stores functions and types for single intervals $\mathbb{Z}^\infty$
   according to the pentagon domains semantics. Beware, this module is NOT generic.
*)
module Interval =
struct
  type t = (ZExt.t * ZExt.t) [@@deriving eq, hash, ord]

  let top () = ((ZExt.NegInfty, ZExt.PosInfty): t)

  let bot () = ((ZExt.PosInfty, ZExt.NegInfty): t)

  let is_top (x:t) = ((ZExt.NegInfty, ZExt.PosInfty) = x)

  let is_bot ((l, u): t) = l > u

  (** Interval intersection *)
  let inter ((l1, u1): t) ((l2, u2): t) =
    let max_lb = if l1 <= l2 then l2 else l1 in
    let min_ub = if u1 <= u2 then u1 else u2 in
    ((max_lb, min_ub): t)

  let add ((l1, u1): t) ((l2, u2): t) =
    let ( + ) = ZExt.add_opt in
    ((Option.default ZExt.NegInfty (l1 + l2), Option.default ZExt.PosInfty (u1 + u2)): t)

  (** Taken from module IArith *)
  let mul ((x1, x2): t) ((y1, y2): t) =
    let ( * ) = ZExt.mul in
    ((
      ZExt.min4 (x1 * y1) (x1 * y2) (x2 * y1) (x2 * y2),
      ZExt.max4 (x1 * y1) (x1 * y2) (x2 * y1) (x2 * y2)
    ): t)

  (** Taken from module IArith *)
  let div ((x1, x2): t) ((y1, y2): t) =
    if y1 <= ZExt.zero && y2 >= ZExt.zero then top() else
      let ( / ) = ZExt.div in
      ((
        ZExt.min4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2),
        ZExt.max4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2)
      ): t)

  let sup (i: t) = if is_bot i then ZExt.NegInfty else snd i;;

  let inf (i: t) = if is_bot i then ZExt.PosInfty else fst i;;

  (** Checks whether the lower bound is -infty, i.e., unbound *)
  (**
     TODO: Verfiy that `inf` is correct here. Alternative `fst`.
  *)
  let no_lowerbound (i: t) = ZExt.NegInfty = inf i

  (** Checks whether the upper bound is +infty, i.e., unbound *)
  (**
     TODO: Verfiy that `sup` is correct here. Alternative `snd`.
  *)
  let no_upperbound (i: t) = ZExt.PosInfty = sup i

  let rem (i1: t) i2 =
    (* i1 % i2 *)
    let (l2, u2) = i2 in
    if l2 <= ZExt.zero && u2 >= ZExt.zero then
      top() 
    else if no_lowerbound i2 || no_upperbound i2 then
      i1
    else
      let ub_minus_1 = ZExt.add_unsafe (ZExt.max (ZExt.abs l2) (ZExt.abs u2)) (ZExt.of_int (-1)) in
      inter i1 ((ZExt.neg ub_minus_1, ub_minus_1): t)


  (**
     Creates a single interval from the supplied integer values.
  *)
  let create i1 i2 = (ZExt.of_int i1, ZExt.of_int i2)

  let leq ((l1, u1): t) ((l2, u2): t) = l2 <= l1 && u1 <= u2

  let union ((l1, u1): t) ((l2, u2): t) = (ZExt.min l1 l2, ZExt.max u1 u2)

  let widen (l1, u1) (l2, u2) =
    let l = if l1 <= l2 then l2 else ZExt.NegInfty in
    let u = if u2 <= u1 then u2 else ZExt.PosInfty in
    (l, u)

  let narrow (i1: t) (i2: t) = 
    inter i1 i2

end

(** Provides functions and types for collections of Interval. *)
module INTV  = 
struct
  type t = Interval.t list [@@deriving eq, hash, ord]

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
    (* If we don't know any variables, i.e. sub = [], then bot = top holds. *)
    sub = [] || 
    existsi (
      fun i set ->
        (* direct contradiction *)
        VarSet.mem i set ||
        (* We assume that the sets inside sub only contain values < length of the list.*)
        VarSet.exists (fun y -> VarSet.mem i (List.at sub y)) set
    ) sub


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

  let forget_vars (vars : int BatList.t) =
    BatList.mapi (fun x ys ->
        if BatList.mem x vars then
          VarSet.empty
        else
          VarSet.filter (fun y -> not (BatList.mem y vars)) ys
      )


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
    (* Results in: {x_1 -> {y1, y2, ..., yn}} *)
    "{" ^ relations_string ^ "}"

  let to_string (sub: t) = 
    if is_bot sub then
      "bot"
    else if is_top sub then
      "top"
    else
      to_string sub 

end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module PNTG =
struct
  type t = { intv: INTV.t; sub: SUB.t } [@@deriving eq, ord]

  let hash : (t -> int)  = fun _ -> failwith "TODO"
  let equal pntg1 pntg2  = INTV.equal pntg1.intv pntg2.intv && SUB.equal pntg1.sub pntg2.sub;;
  let copy (x: t) = x
  let empty () = { intv = []; sub = [] }
  let is_empty pntg =
    match pntg.intv, pntg.sub with
    | [], [] -> true
    | _ -> false

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
        INTV.dim_add dim_change pntg.intv,
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
        INTV.dim_remove dim_change pntg.intv,
        SUB.dim_remove dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)
end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
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

  type t = VarManagement.t [@@deriving eq]

  type var = V.t

  let pretty_diff () (x, y) = failwith "TODO pretty_diff"

  let show varM = failwith "TODO"

  let pretty () (x:t) = failwith "TODO"

  let printXml f x = failwith "TODO"

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

  (**
     Bottom creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: bot = top.
  *)
  let bot () = {d = None; env = empty_env}

  (**
     Top creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: top = bot.
  *)
  let top () = {d = Some {intv = []; sub = []}; env = empty_env}

  let top_of_env env = dimchange2_add (top ()) env

  let is_bot t = 
    match t.d with
    | None -> true
    | Some d -> INTV.is_bot d.intv || SUB.is_bot d.sub

  let is_top t = 
    match t.d with
    | None -> false
    | Some d -> INTV.is_top d.intv && SUB.is_top d.sub


  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {intv = INTV.meet d1'.intv d2'.intv; sub = SUB.meet d1'.sub d2'.sub}; env = sup_env}: t)
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
      let for_all_i f lst =
        List.for_all (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) lst) in
      let bool1 = INTV.leq interval1 interval2 in
      let bool2 = for_all_i(fun x s2x -> 
          SUB.VarSet.for_all(fun y -> 
              let s1x = SUB.VarList.at sub1 x in
              let b1x = BatList.at interval1 x in
              let b1y = BatList.at interval1 y in
              SUB.VarSet.mem y s1x ||
              Interval.sup b1x < Interval.inf b1y
            ) s2x
        ) sub2 in
      bool1 && bool2
    | Some d1', None -> INTV.is_bot d1'.intv || SUB.is_bot d1'.sub
    | _ -> true

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
      let intv_join = INTV.join d1'.intv d1'.intv in
      let s' x s1x = SUB.VarSet.inter s1x (List.at d2'.sub x) in
      let s'' x s1x = SUB.VarSet.filter (fun y -> Interval.sup (List.at d2'.intv x) < Interval.inf (List.at d2'.intv y)) s1x in
      let s''' x = SUB.VarSet.filter (fun y -> Interval.sup (List.at d1'.intv x) < Interval.inf (List.at d1'.intv y)) (List.at d2'.sub x) in
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
      ({d = Some {intv = INTV.widen d1'.intv d2'.intv; sub = SUB.widen d1'.sub d2'.sub}; env = sup_env}: t)
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

  let to_string pntg1 =
    match pntg1.d with
    | None -> "bot"
    | Some d ->
      let intv_str = INTV.to_string d.intv in
      let sub_str = SUB.to_string d.sub in
      Printf.sprintf "Pentagon: %s, %s" intv_str sub_str



  (* S2 Specific functions of RelationDomain *)
  let is_bot_env t = t.d = None

  let forget_vars t vars = 
    if is_bot t || is_bot_env t || vars = [] then t
    else 
      let (pntg: PNTG.t) = Option.get t.d in
      let int_vars = List.map (fun v -> Environment.dim_of_var t.env v) vars in
      {d = Some({intv = INTV.forget_vars int_vars pntg.intv; sub = SUB.forget_vars int_vars pntg.sub}); env=t.env};;


  let z_ext_of_scalar (s: Scalar.t) = 
    match s with
    | Float(f) -> ZExt.of_float f
    | Mpqf(mpqf) -> ZExt.of_float (Mpqf.to_float mpqf)
    | Mpfrf(mpfrf) -> ZExt.of_float (Mpfrf.to_float mpfrf)


  open IntDomain0
  module IArith = IntervalArith (IntOps.BigIntOps)
  let assign_texpr (t: t) var (texp: Texpr1.expr) =
    let exception NotLinear in
    match t.d with
    | None -> bot ()
    | Some d ->
      (**
         TODO: Adjust the environments of the returned pentagons, currently new variables are not added.
      *)

      (* This is the variable we are assigning to *)
      let dim_x = Environment.dim_of_var t.env var in
      let rec convert_texpr (texp: Texpr1.expr) =
        let sub_without_x = SUB.forget_vars [dim_x] d.sub in
        (match texp with
         (** Case: x := [inv.inf, inv.sup] *)
         | Cst (Interval inv) ->
           let intv = BatList.modify_at dim_x (fun _ -> (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup)) d.intv
           in
           ({d=Some({intv = intv; sub = d.sub}); env=t.env}: t)

         (** Case: x := s *)
         | Cst (Scalar s) -> 
           let intv = BatList.modify_at dim_x (fun _ -> (z_ext_of_scalar s, z_ext_of_scalar s)) d.intv
           in
           ({d=Some({intv = intv; sub = d.sub}); env=t.env}: t)

         (** Case: x := y *)
         | Var y ->
           let dim_y = Environment.dim_of_var t.env y in
           let intv = BatList.modify_at dim_x (fun _ -> BatList.at d.intv dim_y) d.intv in
           let sub = d.sub |>
                     SUB.forget_vars [dim_x] |>
                     (* x = y ==> if z < y then also z < x *)
                     SUB.VarList.map (
                       fun set ->
                         if SUB.VarSet.mem dim_y set then
                           SUB.VarSet.add dim_x set
                         else 
                           set
                     ) |>
                     (* SUBs of x := SUBs of y *)
                     BatList.modify_at dim_x (fun _ -> BatList.at d.sub dim_y)
           in
           ({d=Some({intv = intv; sub = sub}); env=t.env}: t)

         | Unop  (Neg,  e, _, _) -> 
           let pntg = convert_texpr e in
           let d = Option.get pntg.d in
           let intv, sub = d.intv, d.sub in
           let intv = BatList.modify_at dim_x (
               fun intv ->
                 (ZExt.neg (Interval.sup intv), ZExt.neg (Interval.inf intv))
             ) intv
           in
           (**
              We do not add redundant information in SUBs. 
              Later checks can derive inequalities by looking at intv.
           *)
           ({d=Some({intv = intv; sub = sub_without_x}); env=t.env}: t)

         | Unop  (Cast, e, _, _) -> convert_texpr e

         | Unop  (Sqrt, e, _, _) ->
           (** What is the semantics of Sqrt. May we still support this? *)
           raise NotLinear

         | Binop (Add, e1, e2, _, _) -> 
           let pntg1 = convert_texpr e1 in
           let pntg2 = convert_texpr e2 in
           let d1, d2 = Option.get pntg1.d, Option.get pntg2.d in
           let intv_1, sub_1 = d1.intv, d1.sub in
           let intv_2, sub_2 = d2.intv, d2.sub in

           let i2 = BatList.at intv_2 dim_x in
           let intv = BatList.modify_at dim_x (
               fun i1 ->
                 Interval.add i1 i2
             ) intv_1
           in
           ({d=Some({intv = intv; sub = sub_without_x}); env=t.env}: t)

         | Binop (Sub, e1, e2, t, r) ->
           convert_texpr (Binop (Add, e1, Unop (Neg, e2, t, r), t, r))

         | Binop (Mul, e1, e2, _, _) ->
           let pntg1 = convert_texpr e1 in
           let pntg2 = convert_texpr e2 in
           let d1, d2 = Option.get pntg1.d, Option.get pntg2.d in
           let intv_1, sub_1 = d1.intv, d1.sub in
           let intv_2, sub_2 = d2.intv, d2.sub in

           let i2 = BatList.at intv_2 dim_x in
           let intv = BatList.modify_at dim_x (
               fun i1 -> Interval.mul i1 i2 ) intv_1 in

           ({d=Some({intv = intv; sub = sub_without_x}); env=t.env}: t)

         | Binop (Div, e1, e2, _, _) ->
           let pntg1 = convert_texpr e1 in
           let pntg2 = convert_texpr e2 in
           let d1, d2 = Option.get pntg1.d, Option.get pntg2.d in
           let intv_1, sub_1 = d1.intv, d1.sub in
           let intv_2, sub_2 = d2.intv, d2.sub in

           let i2 = BatList.at intv_2 dim_x in
           let intv = BatList.modify_at dim_x (
               fun i1 -> Interval.div i1 i2 ) intv_1 in
           (** TODO: Adjust SUBs *)
           ({d=Some({intv = intv; sub = sub_without_x}); env=t.env}: t)

         (** 
            Implemented as described by the paper mention at the beginning of this file.
            Refer to 6.2.2 Remainder.
         *)
         | Binop (Mod, e1, e2, _, _)  ->
           let pntg1 = convert_texpr e1 in
           let pntg2 = convert_texpr e2 in
           let d1, d2 = Option.get pntg1.d, Option.get pntg2.d in
           let intv_1, sub_1 = d1.intv, d1.sub in
           let intv_2, sub_2 = d2.intv, d2.sub in

           let i2 = BatList.at intv_2 dim_x in
           let intv = BatList.modify_at dim_x (
               fun i1 -> 
                 Interval.rem i1 i2
             ) intv_1 in

           let sub = 
             match e2 with
             | Var divisor -> (
                 let dim_divisor = Environment.dim_of_var t.env divisor in 
                 let intv_divisor = BatList.at intv_2 dim_divisor
                 in
                 if (Interval.inf intv_divisor) < ZExt.zero then 
                   sub_without_x
                 else
                   BatList.modify_at dim_x (fun _ -> SUB.VarSet.singleton dim_divisor) sub_without_x
               )
             | _ -> sub_without_x
           in
           ({d=Some({intv = intv; sub = sub}); env=t.env}: t)


         | Binop (Pow, e1, e2, _, _) -> 
           let pntg1 = convert_texpr e1 in
           let pntg2 = convert_texpr e2 in
           top_of_env t.env)
      in
      convert_texpr texp;;


  let assign_texpr t var texp = Timing.wrap "assign_texpr" (assign_texpr t var) texp

  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) = 
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_vars t [var]

  let assign_var t v v' = 
    let t = add_vars t [v; v'] in
    assign_texpr t v (Var v');;


  let assign_var_parallel (t: t) (var_tuples: (var *  var) list) : t = failwith "TODO assign_var_parallel"

  (**
     Combines two var lists into a list of tuples and runs assign_var_parallel
  *)
  let assign_var_parallel' t vs1 vs2 =
    let var_tuples = List.combine vs1 vs2 in
    assign_var_parallel t var_tuples

  let assign_var_parallel_with t (var_tuples: (var * var) list) : unit =  
    let t' = assign_var_parallel t var_tuples in
    t.d <- t'.d;
    t.env <- t'.env;;

  let assert_constraint ask d e negate no_ov : t = failwith "TODO assert constraint"

  let invariant t : Lincons1Set.elt list = failwith "TODO invariant"

  (** Taken from lin2var. *)
  let substitute_exp ask (t:t) var exp no_ov = 
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in 
    let res = assign_exp ask t var exp no_ov in
    forget_vars res [var]
  ;;

  (** Taken from lin2var.  *)
  let unify pntg1 pntg2 = meet pntg1 pntg2

  type marshal = t
  let marshal t = t
  let unmarshal t = t

  let relift t = t

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env t = t.env

  let eval_interval (ask) = Bounds.bound_texpr

  let to_string pntg = 
    if is_bot pntg then
      "bot"
    else if is_top pntg then
      "top"
    else
      match pntg.d with
      | None -> failwith "is_bot should take care of that"
      | Some(d) -> INTV.to_string d.intv ^ " " ^ SUB.to_string d.sub;;

end

module D2: RelationDomain.RD with type var = Var.t =
struct
  module D = D
  module ConvArg = struct
    let allow_global = false
  end
  include SharedFunctions.AssertionModule (D.V) (D) (ConvArg)
  include D
end
