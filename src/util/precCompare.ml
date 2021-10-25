open Prelude
open Pretty

module Comparison =
struct
  type t =
    | Equal
    | MorePrecise of int
    | LessPrecise of int
    | Incomparable of int * int

  let aggregate_same c1 c2 = match c1, c2 with
    | Incomparable (m1, l1), Incomparable (m2, l2) ->
      Incomparable (m1 + m2, l1 + l2)
    | Incomparable (m1, l), MorePrecise m2
    | MorePrecise m2, Incomparable (m1, l) ->
      Incomparable (m1 + m2, l)
    | Incomparable (m, l1), LessPrecise l2
    | LessPrecise l2, Incomparable (m, l1) ->
      Incomparable (m, l1 + l2)
    | MorePrecise m, LessPrecise l
    | LessPrecise l, MorePrecise m ->
      Incomparable (m, l)
    | Equal, c
    | c, Equal ->
      c
    | MorePrecise m1, MorePrecise m2 ->
      MorePrecise (m1 + m2)
    | LessPrecise l1, LessPrecise l2 ->
      LessPrecise (l1 + l2)

  let to_string_infix = function
    | Equal -> "equal to"
    | MorePrecise _ -> "more precise than"
    | LessPrecise _ -> "less precise than"
    | Incomparable _ -> "incomparable to"

  let counts = function
    | Equal -> (0, 0)
    | MorePrecise m -> (m, 0)
    | LessPrecise l -> (0, l)
    | Incomparable (m, l) -> (m, l)
end

module Make (K: Printable.S) (VD: Lattice.S) (KH: Hashtbl.S with type key = K.t) =
struct
  (* TODO: rename variables *)
  module LVH = KH

  let compare ?(name1="left") ~lvh1 ?(name2="right") ~lvh2 =
    let lvh = LVH.merge (fun k v1 v2 -> Some (v1, v2)) lvh1 lvh2 in
    let compared = LVH.map (fun k (v1, v2) ->
        let v1 = v1 |? VD.bot () in
        let v2 = v2 |? VD.bot () in
        let c = match VD.leq v1 v2, VD.leq v2 v1 with
          | true, true -> Comparison.Equal
          | true, false -> Comparison.MorePrecise 1
          | false, true -> Comparison.LessPrecise 1
          | false, false -> Comparison.Incomparable (1, 1)
        in
        let diff () =
          (if VD.leq v1 v2 then nil else dprintf "diff: %a\n" VD.pretty_diff (v1, v2))
          ++
          (if VD.leq v2 v1 then nil else dprintf "reverse diff: %a\n" VD.pretty_diff (v2, v1))
        in
        let msg = Pretty.dprintf "%s %s %s\n  @[%s: %a\n%s\n%s: %a\n%t@]" name1 (Comparison.to_string_infix c) name2 name1 VD.pretty v1 (Comparison.to_string_infix c) name2 VD.pretty v2 diff in
        (c, msg)
      ) lvh
    in
    LVH.iter (fun k (c, msg) ->
        match c with
        | Comparison.Equal -> ()
        | _ ->
          ignore (Pretty.printf "%a: %t\n" K.pretty k (fun () -> msg))
      ) compared;
    let c = LVH.fold (fun _ (c, _) acc -> Comparison.aggregate_same c acc) compared Comparison.Equal in
    let (m, l) = Comparison.counts c in
    let msg = Pretty.dprintf "%s %s %s    (more precise: %d, less precise: %d, total: %d)" name1 (Comparison.to_string_infix c) name2 m l (LVH.length lvh) in
    (c, msg)
end
