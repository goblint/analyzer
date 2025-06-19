(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
module M = Messages
open GobApron
open BatList
open Pretty

(** 
   Extension of the Zarith types and funcitons.
   The values represent arbitrary precision integers and also negative or positive infinity.
*)
module ZExt =
struct
  type t = PosInfty | NegInfty | Arb of Z.t

  let hash (z: t) = 
    match z with
    | PosInfty -> failwith "ZExt.pow: TODO" 
    | NegInfty -> failwith "ZExt.pow: TODO"
    | Arb(z) -> Z.hash z;;

  let equal (z1: t) (z2: t) = 
    (* Printf.printf "ZExt.equal\n"; *)
    match z1, z2 with
    | PosInfty, PosInfty -> true
    | NegInfty, NegInfty -> true
    | Arb(z1), Arb(z2) -> Z.equal z1 z2
    | _ -> false ;;

  let compare (z1: t) (z2: t) = 
    (* Printf.printf "Zext.compare\n"; *)
    match z1, z2 with
    | NegInfty, NegInfty -> 0
    | PosInfty, PosInfty -> 0
    | NegInfty, _ -> -1 
    | _, NegInfty -> 1
    | PosInfty, _ -> 1
    | _, PosInfty -> -1
    | Arb(z1), Arb(z2) -> Z.compare z1 z2;;

  let (<*) z1 z2 = compare z1 z2 < 0;;
  let (>*) z1 z2 = compare z1 z2 > 0;;
  let (=*) z1 z2 = compare z1 z2 = 0;;
  let (<=*) z1 z2 = compare z1 z2 <= 0;;
  let (>=*) z1 z2 = compare z1 z2 >= 0;;
  let (<>*) z1 z2 = compare z1 z2 <> 0;;


  let of_int i = Arb(Z.of_int i)

  let of_float f =
    if Float.is_nan f then 
      failwith "ZExt.of_float: Tried to convert Nan." 
    else if Float.is_finite f then 
      Arb(Z.of_float f) 
    else if Float.signbit f then 
      NegInfty
    else 
      PosInfty

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
    | None -> failwith "ZExt.add_unsafe: Cannot add PosInfty and NegInfty or vice versa."
    | Some(s) -> s

  (** Alias for add_unsafe *)
  let add = add_unsafe

  (** Alias for add z1 (neg z2) *)
  let sub z1 z2 = add z1 (neg z2)

  let rem_add (Arb z1) (Arb z2) =
    let rem = Z.rem z1 z2 in
    if Z.sign rem < 0 then 
      Arb (Z.add rem z2)
    else
      Arb(rem)

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

  let pow z1 z2 =
    if sign z2 < 0 then failwith "ZExt.pow: z2 should be non negative" else
      match z1, z2 with
      | Arb z1, Arb z2 when Z.sign z1 < 0 && not (Z.fits_nativeint z2) -> 
        if Z.is_even z2 then PosInfty else NegInfty
      | Arb z1, Arb z2 when Z.sign z1 > 0 && not (Z.fits_nativeint z2) -> PosInfty
      | _, Arb z when Z.of_int 0 = z -> (of_int 1)
      | Arb z, _ when Z.of_int 0 = z -> zero 
      | Arb z, _ when Z.of_int 1 = z -> (of_int 1)
      | Arb z1, Arb z2 -> (Arb(Z.pow z1 (Z.to_int z2)))
      | z1, PosInfty when sign z1 < 0 -> failwith "ZExt.pow: Cannot determine whether result is NegInfty or PosInfty (or -1 or 1 for z1 = -1) -> depends on the side of the interval"
      | PosInfty, _ | _, PosInfty -> PosInfty
      | NegInfty, Arb z -> if Z.is_even z then PosInfty else NegInfty
      | _, NegInfty -> failwith "This shouldn't happen (caught in second line of ZExt.pow)"

  let abs z1 = if z1 < zero then neg z1 else z1;;

  let max z1 z2 = if z1 > z2 then z1 else z2;;

  let min z1 z2 = if z1 < z2 then z1 else z2;;

  (** Taken from module IArith *)
  let min4 a b c d = min (min a b) (min c d)

  (** Taken from module IArith *)
  let max4 a b c d = max (max a b) (max c d)

end

module ZExtOps =
struct 

  let (<*) = ZExt.(<*);;
  let (>*) = ZExt.(>*);;
  let (=*) = ZExt.(=*);;
  let (<=*) = ZExt.(<=*);;
  let (>=*) = ZExt.(>=*);;
  let (<>*) = ZExt.(<>*);;

end

(**
   Stores functions and types for single intervals $\mathbb{Z}^\infty$
   according to the pentagon domains semantics. Beware, this module is NOT generic.
*)
module Intv =
struct
  include ZExtOps

  type t = (ZExt.t * ZExt.t) [@@deriving eq, hash, ord]

  let top () = ((ZExt.NegInfty, ZExt.PosInfty): t)

  let bot () = ((ZExt.PosInfty, ZExt.NegInfty): t)

  let is_top (x:t) = ((ZExt.NegInfty, ZExt.PosInfty) = x)

  let is_bot ((l, u): t) = u <* l 

  (** Intv intersection *)
  let inter ((l1, u1): t) ((l2, u2): t) =
    let max_lb = if l1 <=* l2 then l2 else l1 in
    let min_ub = if u1 <=* u2 then u1 else u2 in
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
    if y1 <=* ZExt.zero && y2 >=* ZExt.zero then top() else
      let ( / ) = ZExt.div in
      ((
        ZExt.min4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2),
        ZExt.max4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2)
      ): t)


  (** Checks for bot interval. *)
  let sup (i: t) = if is_bot i then ZExt.NegInfty else snd i;;

  (** Checks for bot interval. *)
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
    if l2 <=* ZExt.zero && u2 >=* ZExt.zero then
      top() 
    else if no_lowerbound i2 || no_upperbound i2 then
      i1
    else
      let ub_minus_1 = ZExt.add (ZExt.max (ZExt.abs l2) (ZExt.abs u2)) (ZExt.of_int (-1)) in
      inter i1 ((ZExt.neg ub_minus_1, ub_minus_1): t)


  (**
     We assume that i1 and i2 are well-formed, i.e. not bot/empty.
  *)
  let pow ((l1, u1): t) ((l2, u2): t) =
    if l2 <* ZExt.zero then top () (* x ^ (-1) is unsupported operation on ints ==> we treat it as undefined behavior, same as division by 0 *)
    else
      match u2 with
      | PosInfty -> if l1 <=* ZExt.of_int (-2) then top () (* can create arbitrarily big numbers with (-2) ^ x *)
        else if l1 >=* ZExt.zero then (ZExt.pow l1 l2, ZExt.pow u1 PosInfty)
        else (* l1 = -1 *)
        if u1 =* ZExt.of_int (-1) then (ZExt.of_int (-1), ZExt.of_int 1)
        else (ZExt.of_int (-1), ZExt.pow u1 PosInfty)
      | NegInfty -> failwith "Intv.pow should not happen"
      | Arb u2z when l1 <* ZExt.zero ->
        if l2 =* u2 then (* special case because we don't have an even AND an odd number ==> either impossible to mirror negative numbers or everything gets nonnegative *)
          let exp = l2 in
          if exp =* ZExt.zero then (ZExt.of_int 1, ZExt.of_int 1) else
          if Z.is_even u2z then
            if u1 >=* ZExt.zero then
              (* i1 contains negative and nonnegative numbers, exp != 0 is even ==> lb = 0, ub depends on greater abs value of bounds *)
              let max_abs = ZExt.max (ZExt.abs l1) u1 in
              let u = ZExt.pow max_abs exp in
              (ZExt.zero, u) else
              (* x -> x ^ n is monotonically decreasing for even n and negative x *)
              let l = ZExt.pow u1 exp in
              let u = ZExt.pow l1 exp in
              (l, u)
          else (* exp is odd *)
            (* x -> x ^ n is monotonically increasing for odd n *)
            (ZExt.pow l1 exp, ZExt.pow u1 exp)
        else
          (* we have at least one even and one odd number in the exponent ==> negative numbers can be mirrored if needed *)
          let greatest_even = if Z.is_even u2z then u2 else ZExt.sub u2 (ZExt.of_int 1) in
          let greatest_odd = if Z.is_odd u2z then u2 else ZExt.sub u2 (ZExt.of_int 1) in
          let l = ZExt.pow l1 greatest_odd in
          let u' = ZExt.pow l1 greatest_even in
          let u'' = if ZExt.sign u1 > 0 then ZExt.pow u1 u2 else u' in
          (l, ZExt.max u' u'')
      | _ -> (* i1 is nonnegative ==> no special cases here :) *)
        let l = ZExt.pow l1 l2 in
        let u = ZExt.pow u1 u2 in
        (l, u)


  (**
     Creates a single interval from the supplied integer values.
  *)
  let create i1 i2 = (ZExt.of_int i1, ZExt.of_int i2)

  let leq ((l1, u1): t) ((l2, u2): t) = l2 <=* l1 && u1 <=* u2

  let union ((l1, u1): t) ((l2, u2): t) = (ZExt.min l1 l2, ZExt.max u1 u2)

  let widen (l1, u1) (l2, u2) =
    let l = if l1 <=* l2 then l2 else ZExt.NegInfty in
    let u = if u2 <=* u1 then u2 else ZExt.PosInfty in
    (l, u)

  let narrow (i1: t) (i2: t) = 
    inter i1 i2


  let neg intv = (ZExt.neg (sup intv), ZExt.neg (inf intv))

  let sub intv_1 intv_2 =
    let intv_2 =  neg intv_2 in
    add intv_1 intv_2

end

(** Provides functions and types for collections of Intv. *)
module Boxes  = 
struct
  type t = Intv.t list [@@deriving eq, ord]
  include ZExtOps
  let is_bot (i: t) = 
    (* Printf.printf "boxes.is_bot\n"; *)
    BatList.exists Intv.is_bot i

  let is_top i =
    (* Printf.printf "boxes.is_top\n"; *)
    BatList.for_all Intv.is_top i

  let to_string (intervals: t) =

    (** Special strings for the bounds of a 32 integer. *)
    let min_or_max_int z = if z =* (ZExt.of_int (-2147483648)) then 
        "-I32" 
      else if z =* (ZExt.of_int (2147483647)) then 
        "+I32"
      else
        ZExt.to_string z
    in
    let string_of_interval i (lb, ub) =
      Printf.sprintf "%i->[%s, %s]" i (min_or_max_int lb) (min_or_max_int ub)
    in
    let bot_or_top =
      if is_bot intervals then
        "(⊥)"
      else if is_top intervals then
        "(⊤)"
      else 
        "( )"
    in
    Printf.sprintf "%s { %s }" bot_or_top (String.concat "; " (List.mapi string_of_interval intervals))

  let equal boxes1 boxes2 =
    BatList.for_all2 (Intv.equal) boxes1 boxes2

  let leq i1 i2 =
    (* Printf.printf "boxes.leq\n"; *)
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
    (*Printf.printf "Boxes.dim_add\n";*)
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


  (* Backup implementation, if dim_change.dim is not sorted and contains duplicates. *)
  (*let dim_remove (dim_change: Apron.Dim.change) (intervals : t) =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      List.filteri (fun i _ -> not (Array.mem i dim_change.dim)) intervals*)


  (* precondition: dim_change is sorted and has unique elements *)
  let dim_remove (dim_change: Apron.Dim.change) (intervals : t) =
    (*Printf.printf "Boxes.dim_remove\n";*)
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

end

(** Stores functions and types for strict upper bounds. *)
module Sub =
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


  let to_string (sub: t) =
    (* Results in: { y1, y2, ..., yn }*)
    let set_string set = "{" ^ (
        VarSet.to_list set |>
        List.map (Idx.to_string) |>
        String.concat ", "
      ) ^ "}" in
    (* Results in: x_1 -> {y1, y2, ..., yn} *)
    String.concat "; " (
      VarList.mapi (
        fun i set ->
          Printf.sprintf "%s->%s" (Idx.to_string i) (set_string set)
      ) sub
    )

  let to_string (sub: t) =
    let bot_or_top = 
      if is_bot sub then
        "(⊥)"
      else if is_top sub then
        "(⊤)"
      else
        "( )"
    in
    Printf.sprintf "%s { %s }" bot_or_top (to_string sub)

end


module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module PNTG =
struct
  type t = { boxes: Boxes.t; sub: Sub.t } [@@deriving eq, ord]

  let hash : (t -> int)  = fun _ -> 1
  let equal pntg1 pntg2  = Boxes.equal pntg1.boxes pntg2.boxes && Sub.equal pntg1.sub pntg2.sub;;
  let copy (x: t) = x
  let empty () = { boxes = []; sub = [] }
  let is_empty pntg =
    match pntg.boxes, pntg.sub with
    | [], [] -> true
    | _ -> false

  let to_string pntg =
    Printf.sprintf "{ boxes = %s; sub = %s }" (Boxes.to_string pntg.boxes) (Sub.to_string pntg.sub)


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
        Boxes.dim_add dim_change pntg.boxes,
        Sub.dim_add dim_change pntg.sub 
      in
      Printf.printf "add %i dims: %i -> %i\n" dim_change.intdim (List.length pntg.boxes) (List.length intv);
      ({boxes = intv; sub = sub}: t)

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
        Boxes.dim_remove dim_change pntg.boxes,
        Sub.dim_remove dim_change pntg.sub 
      in
      Printf.printf "remove %i dims: %i -> %i\n" dim_change.intdim (List.length pntg.boxes) (List.length intv);
      ({boxes = intv; sub = sub}: t)
end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  include SharedFunctions.VarManagementOps (PNTG)
end

let z_ext_of_scalar (s: Scalar.t) = 
  match s with
  | Float(f) -> ZExt.of_float f
  | Mpqf(mpqf) -> ZExt.of_float (Mpqf.to_float mpqf)
  | Mpfrf(mpfrf) -> ZExt.of_float (Mpfrf.to_float mpfrf)

let eval_texpr_to_intv (t: VarManagement.t) (texpr:Texpr1.expr) : Intv.t = 
  let get_dim var = Environment.dim_of_var t.env var in
  let d = Option.get t.d in
  let boxes = d.boxes in
  let rec aux texpr =
    match texpr with
    | Texpr1.Cst (Interval inv) -> (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup)
    | Cst (Scalar s) -> let s = z_ext_of_scalar s in (s, s)
    | Var y -> List.at boxes (get_dim y) 
    | Unop  (Cast, e, _, _) -> aux e
    | Unop  (Sqrt, e, _, _) -> failwith "Do the sqrt. :)"
    | Unop  (Neg,  e, _, _) -> Intv.neg (aux e)
    | Binop (Add, e1, e2, _, _) -> Intv.add (aux e1) (aux e2)
    | Binop (Sub, e1, e2, _, _) -> Intv.sub (aux e1) (aux e2)
    | Binop (Mul, e1, e2, _, _) -> Intv.mul (aux e1) (aux e2)
    | Binop (Div, e1, e2, _, _) -> Intv.div (aux e1) (aux e2)
    | Binop (Mod, e1, e2, _, _)  -> Intv.rem (aux e1) (aux e2)
    | Binop (Pow, e1, e2, _, _) -> Intv.pow (aux e1) (aux e2)
  in
  aux texpr;;

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr =
    match (eval_texpr_to_intv t (Texpr1.to_expr texpr)) with
    | Arb s1, Arb s2 -> Some(s1), Some(s2)
    | Arb s1, _ -> Some(s1), None
    | _, Arb s2 -> None, Some(s2)
    | _, _ -> None, None

  let bound_texpr d texpr1 = Timing.wrap "bounds calculation" (bound_texpr d) texpr1
end

module D =
struct
  include ZExtOps
  include Printable.Std
  include VarManagement
  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  type t = VarManagement.t

  let equal t1 t2 =
    Environment.equal t1.env t2.env
    &&
    match t1.d, t2.d with
    | None, None -> true
    | None, _ -> false
    | _ , None -> false
    | Some(d1), Some(d2) -> PNTG.equal d1 d2


  type var = V.t
  let name () = "pentagon"

  let is_bot t = 
    match t.d with
    | None -> true
    | Some d -> Boxes.is_bot d.boxes || Sub.is_bot d.sub

  let is_top t = 
    match t.d with
    | None -> false
    | Some d -> Boxes.is_top d.boxes && Sub.is_top d.sub

  let to_string pntg =
    if is_bot pntg then
      "⊥"
    else if is_top pntg then
      "⊤"
    else
      (* d = None should have been handled by is_bot. *)
      let d = Option.get pntg.d in
      PNTG.to_string d

  let show = to_string

  let pretty () (t:t) = text (show t)


  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y


  let printXml f (t:t) =  BatPrintf.fprintf f "<value>\n<map>\n<key>\npntg\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%a</value>\n</map>\n</value>\n" (XmlUtil.escape (show t)) Environment.printXml t.env


  let to_yojson t = failwith "TODO"

  (**
     Bottom creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: bot = top.
  *)
  let bot () = {d = None; env = empty_env}

  let bot_of_env env = ({ d = None; env = env }:t)


  (**
     Top creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: top = bot.
  *)
  let top () = {d = Some {boxes = []; sub = []}; env = empty_env}

  let top_of_env env = dimchange2_add (top ()) env

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {boxes = Boxes.meet d1'.boxes d2'.boxes; sub = Sub.meet d1'.sub d2'.sub}; env = sup_env}: t)
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
      let boxes_1, boxes_2 = d1'.boxes, d2'.boxes in
      let sub1, sub2 = d1'.sub, d2'.sub in
      let for_all_i f lst =
        List.for_all (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) lst) in
      let bool1 = Boxes.leq boxes_1 boxes_2 in
      let bool2 = for_all_i(fun x s2x -> 
          Sub.VarSet.for_all(fun y -> 
              let s1x = Sub.VarList.at sub1 x in
              let b1x = BatList.at boxes_1 x in
              let b1y = BatList.at boxes_1 y in
              Sub.VarSet.mem y s1x ||
              Intv.sup b1x <* Intv.inf b1y
            ) s2x
        ) sub2 in
      bool1 && bool2
    | Some d1', None -> Boxes.is_bot d1'.boxes || Sub.is_bot d1'.sub
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
      let joined_boxes = Boxes.join d1'.boxes d1'.boxes in
      let s' x s1x = Sub.VarSet.inter s1x (List.at d2'.sub x) in
      let s'' x s1x = Sub.VarSet.filter (fun y -> Intv.sup (List.at d2'.boxes x) < Intv.inf (List.at d2'.boxes y)) s1x in
      let s''' x = Sub.VarSet.filter (fun y -> Intv.sup (List.at d1'.boxes x) < Intv.inf (List.at d1'.boxes y)) (List.at d2'.sub x) in
      let joined_sub = List.mapi (fun x s1x -> Sub.VarSet.union (s' x s1x) (Sub.VarSet.union (s'' x s1x) (s''' x))) d1'.sub in
      ({d = Some {boxes = joined_boxes; sub = joined_sub}; env = sup_env}: t)
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
      ({d = Some {boxes = Boxes.widen d1'.boxes d2'.boxes; sub = Sub.widen d1'.sub d2'.sub}; env = sup_env}: t)
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

  (* S2 Specific functions of RelationDomain *)
  let is_bot_env t = t.d = None

  let forget_vars t vars = 
    if is_bot t || is_bot_env t || vars = [] then t
    else 
      let (pntg: PNTG.t) = Option.get t.d in
      let int_vars = List.map (fun v -> Environment.dim_of_var t.env v) vars in
      {d = Some({boxes = Boxes.forget_vars int_vars pntg.boxes; sub = Sub.forget_vars int_vars pntg.sub}); env=t.env};;


  let assign_texpr (t: t) var (texp: Texpr1.expr) =
    Printf.printf "assign_texpr\n";

    let dim = Environment.dim_of_var t.env var in

    let rec aux (texp: Texpr1.expr) t : Boxes.t * Sub.t =
      match t.d with
      | None -> ([], []) (** Bot *)
      | Some d ->
        let boxes, sub = d.boxes, d.sub in

        let sub_without_var = Sub.forget_vars [dim] sub in
        let boxes_without_var = Boxes.forget_vars [dim] boxes in
        (match texp with
         (** Case: x := [inv.inf, inv.sup] *)
         | Cst (Interval inv) ->

           Printf.printf "assign_texpr: Cst (Interval inv)\n";
           let boxes = BatList.modify_at dim (fun _ -> (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup)) boxes
           in
           (boxes, sub)

         (** Case: x := s *)
         | Cst (Scalar s) -> 
           Printf.printf "assign_texpr: Cst (Scalar s)\n";

           let boxes = BatList.modify_at dim (fun _ -> (z_ext_of_scalar s, z_ext_of_scalar s)) boxes
           in
           (boxes, sub)

         (** Case: x := y *)
         | Var y ->
           Printf.printf "assign_texpr: Var y\n";
           let dim_y = Environment.dim_of_var t.env y in
           let boxes = BatList.modify_at dim (fun _ -> BatList.at boxes dim_y) boxes in
           let sub = sub |>
                     Sub.forget_vars [dim] |>
                     (* x = y ==> if z < y then also z < x *)
                     Sub.VarList.map (
                       fun set ->
                         if Sub.VarSet.mem dim_y set then
                           Sub.VarSet.add dim set
                         else 
                           set
                     ) |>
                     (* Subs of x := Subs of y *)
                     BatList.modify_at dim (fun _ -> BatList.at sub dim_y)
           in
           (boxes, sub)

         | Unop  (Neg,  e, _, _) -> 
           Printf.printf "assign_texpr: Unop (Neg)\n";
           let (boxes, sub) = aux e t in

           let boxes = BatList.modify_at dim (
               fun intv ->
                 (ZExt.neg (Intv.sup intv), ZExt.neg (Intv.inf intv))
             ) boxes
           in
           (**
              We do not add redundant information in Subs. 
              Later checks can derive inequalities by looking at Boxes.
           *)
           (boxes, sub_without_var)

         | Unop  (Cast, e, _, _) -> 
           Printf.printf "assign_texpr: Unop (Cast)\n";aux e t

         | Unop  (Sqrt, e, _, _) ->

           Printf.printf "assign_texpr: Unop (Sqrt)\n";
           (** 
              TODO: What is the semantics of Sqrt. May we still support this? 
           *)
           (boxes_without_var, sub_without_var)
         | Binop (Add, e1, e2, _, _) ->

           Printf.printf "assign_texpr: BinOp (Add)\n";
           let (boxes_1, sub_1) = aux e1 t in
           let (boxes_2, sub_2) = aux e2 t in

           let i2 = BatList.at boxes_2 dim in
           let boxes = BatList.modify_at dim (
               fun i1 ->
                 Intv.add i1 i2
             ) boxes_1
           in
           (boxes, sub_without_var)

         | Binop (Sub, e1, e2, t0, r) ->
           Printf.printf "assign_texpr: BinOp (Sub)\n";
           aux (Binop (Add, e1, Unop (Neg, e2, t0, r), t0, r)) t

         | Binop (Mul, e1, e2, _, _) ->
           Printf.printf "assign_texpr: BinOp (Mul)\n";
           let (boxes_1, sub_1) = aux e1 t in
           let (boxes_2, sub_2) = aux e2 t in

           let i2 = BatList.at boxes_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> Intv.mul i1 i2 ) boxes_1 in
           (intv, sub_without_var)

         | Binop (Div, e1, e2, _, _) ->
           Printf.printf "assign_texpr: BinOp (Div)\n";
           let (boxes_1, sub_1) = aux e1 t in
           let (boxes_2, sub_2) = aux e2 t in

           let i2 = BatList.at boxes_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> Intv.div i1 i2 ) boxes_1 in
           (intv, sub_without_var)

         (** 
            Implemented as described by the paper mention at the beginning of this file.
            Refer to 6.2.2 Remainder.
         *)
         | Binop (Mod, e1, e2, _, _)  ->
           Printf.printf "assign_texpr: BinOp (Mod)\n";
           let (boxes_1, sub_1) = aux e1 t in
           let (boxes_2, sub_2) = aux e2 t in

           let i2 = BatList.at boxes_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> 
                 Intv.rem i1 i2
             ) boxes_1 in

           let sub = 
             match e2 with
             | Var divisor -> (
                 let dim_divisor = Environment.dim_of_var t.env divisor in 
                 let intv_divisor = BatList.at boxes_2 dim_divisor
                 in
                 if (Intv.inf intv_divisor) <* ZExt.zero then 
                   sub_without_var
                 else
                   BatList.modify_at dim (fun _ -> Sub.VarSet.singleton dim_divisor) sub_without_var
               )
             | _ -> sub_without_var
           in
           (intv, sub)


         (** e1 ^ e2 *)
         | Binop (Pow, e1, e2, _, _) -> 
           Printf.printf "assign_texpr: BinOp (Pow)\n";
           let (boxes_1, sub_1) = aux e1 t in
           let (boxes_2, sub_2) = aux e2 t in

           let i2 = BatList.at boxes_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> 
                 Intv.pow i1 i2
             ) boxes_1 in

           (intv, sub_without_var)
        ) in

    let (boxes, sub) = aux texp t in
    match boxes, sub with
    | [], [] -> { d= None; env=t.env }
    | _ ->
      { d=Some({ boxes = boxes; sub = sub }); env = t.env }
  ;;


  (* let assign_texpr t var texp = Timing.wrap "assign_texpr" (assign_texpr t var) texp *)

  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) = 
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_vars t [var]

  let assign_var t v v' = 
    let t = add_vars t [v; v'] in
    assign_texpr t v (Var v');;


  let assign_var_parallel (t: t) (var_tuples: (var *  var) list) : t = 
    let assigned_vars = List.map fst var_tuples in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars var_tuples in
    match multi_t.d with
    | Some m when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      remove_vars switched_arr primed_vars
    | _ -> t


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


  let string_of_texpr1 (texpr: Texpr1.expr) =
    let rec aux texpr = 
      match texpr with
      | Texpr1.Cst (Interval inv) -> "Cst(Interval inv)"
      | Cst (Scalar s) -> Scalar.to_string s
      | Var y -> "Var " ^ Var.to_string y 
      | Unop  (Neg,  e, _, _) -> "Neg(" ^ aux e ^ ")"
      | Unop  (Cast, e, _, _) -> "Cast(" ^ aux e ^ ")"
      | Unop  (Sqrt, e, _, _) -> "Sqrt(" ^ aux e ^ ")"
      | Binop (Add, e1, e2, _, _) -> "Add(" ^ aux e1 ^ ", " ^ aux e2 ^ ")"
      | Binop (Sub, e1, e2, _, _) -> "Sub(" ^ aux e1 ^ ", " ^ aux e2 ^ ")"
      | Binop (Mul, e1, e2, _, _) -> "Mul(" ^ aux e1 ^ ", " ^ aux e2 ^ ")"
      | Binop (Div, e1, e2, _, _) -> "Div(" ^ aux e1 ^ ", " ^ aux e2 ^ ")"
      | Binop (Mod, e1, e2, _, _) -> "Mod(" ^ aux e1 ^ ", " ^ aux e2 ^ ")"
      | Binop (Pow, e1, e2, _, _) -> "Pow(" ^ aux e1 ^ ", " ^ aux e2 ^ ")"
    in
    aux texpr

  let string_of_texpr_tcons1 texpr (tcons1) =
    string_of_texpr1 texpr ^ " " ^  Tcons1.string_of_typ (Tcons1.get_typ tcons1) ^ " 0"



  (**
      Taken from Lin2Var.
  *)
  let assert_constraint ask t e negate (no_ov: bool Lazy.t) =
    Printf.printf "assert_constraint\n";
    Printf.printf "%s\n\n" (to_string t);

    let zero = ZExt.zero in 
    (** Checks if the constraining interval violates the assertion. *)
    let interval_helper ((lb, ub): ZExt.t * ZExt.t) (tcons_typ: Tcons1.typ) =
      match tcons_typ with
      | EQ when lb <=* zero && ub >=* zero -> t
      | SUPEQ when ub >=* zero -> t
      | SUP when ub >* zero -> t
      | DISEQ when ub <>* zero || lb <>* zero -> t
      | EQMOD (s) -> (
          let s = z_ext_of_scalar s in
          let ( - ) = ZExt.sub in
          if (ub - lb) <= (s - ZExt.of_int 2) && lb <>* zero && (ZExt.rem_add lb s) <=* (ZExt.rem_add ub s) then
            bot_of_env t.env
          else
            t
        )
      | _ -> bot_of_env t.env
    in
    let var_intv_meet constraining_interval dim_x : t = (
      (* Already matched to be not None. *)
      let d = Option.get t.d in
      let intv_x = List.at d.boxes dim_x in
      let intersected_intv_x = Intv.inter intv_x constraining_interval in
      if Intv.is_bot intersected_intv_x then
        bot_of_env t.env
      else 
        let boxes = List.modify_at dim_x (fun _ -> intersected_intv_x) d.boxes in
        { d = Some({boxes = boxes; sub = d.sub}); env=t.env}
    ) in
    match t.d with 
    | None -> t
    | Some d ->
      match Convert.tcons1_of_cil_exp ask t t.env e negate (Lazy.from_val true) with
      | exception Convert.Unsupported_CilExp exn -> Printf.printf "failed to convert cil expression: exception %s\n" (SharedFunctions.show_unsupported_cilExp exn); t
      | tcons1 -> 
        let tcons_typ = Tcons1.get_typ tcons1 in
        let texpr = (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons1) in
        Printf.printf "%s\n" (string_of_texpr_tcons1 texpr tcons1);

        let rec assert_constraint t (texpr: Texpr1.expr) =
          match texpr with 
          | Cst (Interval inv) -> 
            interval_helper (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup) tcons_typ
          | Cst (Scalar s) -> 
            interval_helper (z_ext_of_scalar s, z_ext_of_scalar s) tcons_typ
          | Var x -> (
              (** We ignore sub-information for now. *)
              let dim_x = Environment.dim_of_var t.env x in
              let intv_x = List.at d.boxes dim_x in
              let (lb, ub) = intv_x in
              let zero = ZExt.zero in
              match tcons_typ with
              | EQ -> var_intv_meet (zero, zero) dim_x
              | SUPEQ -> var_intv_meet (zero, PosInfty) dim_x
              | SUP -> var_intv_meet (ZExt.of_int 1, PosInfty) dim_x
              | DISEQ ->
                if lb <>* zero && ub <>* zero then
                  t
                else if lb =* zero && ub >* zero then
                  var_intv_meet (ZExt.of_int 1, PosInfty) dim_x
                else if lb <* zero && ub =* zero then
                  var_intv_meet (NegInfty, ZExt.of_int (-1)) dim_x
                else 
                  bot_of_env t.env
              | EQMOD (s) -> (
                  let t = interval_helper intv_x tcons_typ in
                  let s = z_ext_of_scalar s in
                  match t.d with
                  | None -> t
                  | Some(pntg) -> (
                      let corrected_intv = (
                        let ( - ) = ZExt.sub in
                        let ( + ) = ZExt.add in
                        let tmp_lb = lb - ZExt.rem_add lb s in
                        let lb = if tmp_lb <* lb then (tmp_lb) + s else tmp_lb in
                        let ub = ub - ZExt.rem_add ub s in
                        (lb, ub)
                      )
                      in
                      let boxes = List.modify_at dim_x (fun _ -> corrected_intv) d.boxes in
                      ({ d = Some({boxes=boxes; sub=d.sub}); env=t.env})
                    )
                )
            )
          | Binop (Sub, e1, e2, t0, r) -> (
              match e1, e2 with
              | Var x, Cst(Scalar s) when (z_ext_of_scalar s) =* zero -> assert_constraint t e1

              | Var x, e -> (
                  let inf = Intv.inf in
                  let intv = eval_texpr_to_intv t (e) in
                  let dim_x = Environment.dim_of_var t.env x in

                  match tcons_typ with
                  | EQ -> var_intv_meet intv dim_x
                  | SUPEQ -> var_intv_meet (Intv.inf intv, PosInfty) dim_x
                  | SUP -> var_intv_meet (ZExt.add (Intv.inf intv) (ZExt.of_int 1), PosInfty) dim_x
                  | DISEQ when Intv.inf intv = Intv.sup intv -> (
                      let c = inf intv in
                      let (lb_x, ub_x) = List.at d.boxes dim_x in

                      if c = lb_x && c = ub_x then
                        bot_of_env t.env
                      else if c = lb_x then
                        var_intv_meet (ZExt.add c (ZExt.of_int 1), PosInfty) dim_x
                      else if c = ub_x then
                        var_intv_meet (NegInfty ,ZExt.sub c (ZExt.of_int 1)) dim_x
                      else
                        t

                    )
                  | DISEQ -> t 
                  | EQMOD(s) -> t
                )

              | Cst(Scalar s), Var x when (z_ext_of_scalar s) =* zero && (tcons_typ = DISEQ || tcons_typ = EQ) -> assert_constraint t e2

              | e, Var x -> (let inf = Intv.inf in
                             let intv = eval_texpr_to_intv t (e) in
                             let dim_x = Environment.dim_of_var t.env x in

                             match tcons_typ with
                             | EQ -> var_intv_meet intv dim_x
                             | SUPEQ -> var_intv_meet (NegInfty, Intv.sup intv) dim_x
                             | SUP -> var_intv_meet (NegInfty, ZExt.sub (Intv.sup intv) (ZExt.of_int 1)) dim_x
                             | DISEQ when Intv.inf intv = Intv.sup intv -> (
                                 let c = inf intv in
                                 let (lb_x, ub_x) = List.at d.boxes dim_x in

                                 if c = lb_x && c = ub_x then
                                   bot_of_env t.env
                                 else if c = lb_x then
                                   var_intv_meet (ZExt.add c (ZExt.of_int 1), PosInfty) dim_x
                                 else if c = ub_x then
                                   var_intv_meet (NegInfty ,ZExt.sub c (ZExt.of_int 1)) dim_x
                                 else
                                   t

                               )
                             | DISEQ -> t 
                             | EQMOD(s) -> t
                            )

              | _ -> let intv = eval_texpr_to_intv t (Binop (Sub, e1, e2, t0, r)) in interval_helper intv tcons_typ
            )
          | _ -> t
        in
        assert_constraint t texpr

  let invariant t : Lincons1Set.elt list = []

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

  let to_string t = 
    if is_bot t then
      "⊥"
    else if is_top t then
      "⊤"
    else
      match t.d with
      | None -> failwith "is_bot should take care of that"
      | Some(d) -> PNTG.to_string d;;

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
