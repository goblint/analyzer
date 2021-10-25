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

module Make (K: Printable.S) (D: Lattice.S) (KH: Hashtbl.S with type key = K.t) =
struct

  let compare ?(name1="left") ?(name2="right") kh1 kh2 =
    let kh = KH.merge (fun k v1 v2 -> Some (v1, v2)) kh1 kh2 in
    let compared = KH.map (fun k (v1, v2) ->
        let v1 = v1 |? D.bot () in
        let v2 = v2 |? D.bot () in
        let c = match D.leq v1 v2, D.leq v2 v1 with
          | true, true -> Comparison.Equal
          | true, false -> Comparison.MorePrecise 1
          | false, true -> Comparison.LessPrecise 1
          | false, false -> Comparison.Incomparable (1, 1)
        in
        let diff () =
          (if D.leq v1 v2 then nil else dprintf "diff: %a\n" D.pretty_diff (v1, v2))
          ++
          (if D.leq v2 v1 then nil else dprintf "reverse diff: %a\n" D.pretty_diff (v2, v1))
        in
        let msg = Pretty.dprintf "%s %s %s\n  @[%s: %a\n%s\n%s: %a\n%t@]" name1 (Comparison.to_string_infix c) name2 name1 D.pretty v1 (Comparison.to_string_infix c) name2 D.pretty v2 diff in
        (c, msg)
      ) kh
    in
    KH.iter (fun k (c, msg) ->
        match c with
        | Comparison.Equal -> ()
        | _ ->
          ignore (Pretty.printf "%a: %t\n" K.pretty k (fun () -> msg))
      ) compared;
    let c = KH.fold (fun _ (c, _) acc -> Comparison.aggregate_same c acc) compared Comparison.Equal in
    let (m, l) = Comparison.counts c in
    let msg = Pretty.dprintf "%s %s %s    (more precise: %d, less precise: %d, total: %d)" name1 (Comparison.to_string_infix c) name2 m l (KH.length kh) in
    (c, msg)
end
