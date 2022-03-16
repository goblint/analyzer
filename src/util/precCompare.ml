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

module Make (D: Lattice.S) =
struct

  let compare ?(verbose=false) ?(name1="left") ?(name2="right") v1 v2 =
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
    let msg = if verbose then Pretty.dprintf "%s %s %s\n  @[%s: %a\n%s\n%s: %a\n%t@]" name1 (Comparison.to_string_infix c) name2 name1 D.pretty v1 (Comparison.to_string_infix c) name2 D.pretty v2 diff
      else Pretty.nil in
    (c, msg)
end

module MakeHashtbl (K: Printable.S) (D: Lattice.S) (KH: Hashtbl.S with type key = K.t) =
struct

  module CompareD = Make (D)

  let compare ?(verbose=false) ?(name1="left") ?(name2="right") kh1 kh2 =
    let kh = KH.merge (fun k v1 v2 -> Some (v1, v2)) kh1 kh2 in
    let compared = KH.map (fun k (v1, v2) ->
        let v1 = v1 |? D.bot () in
        let v2 = v2 |? D.bot () in
        CompareD.compare ~verbose ~name1 ~name2 v1 v2
      ) kh
    in
    KH.iter (fun k (c, msg) ->
        match c with
        | Comparison.Equal -> ()
        | _ ->
          if verbose then ignore (Pretty.printf "%a: %t\n" K.pretty k (fun () -> msg))
      ) compared;
    let c = KH.fold (fun _ (c, _) acc -> Comparison.aggregate_same c acc) compared Comparison.Equal in
    let (m, l) = Comparison.counts c in
    let msg = Pretty.dprintf "%s %s %s    (more precise: %d, less precise: %d, total: %d)" name1 (Comparison.to_string_infix c) name2 m l (KH.length kh) in
    (c, msg)
end

module MakeDump (Util: PrecCompareUtil.S) =
struct
  (* open! Defaults (* CircInterval / Enums / ... need initialized conf *) *)
  open! Batteries
  open Util

  let load filename =
    let f = open_in_bin filename in
    let dump: dump = Marshal.from_channel f in
    let dump: result = {name = dump.name; results = unmarshal dump.marshalled } in
    close_in_noerr f;
    dump

  module CompareDump = MakeHashtbl (Key) (Dom) (RH)

  let compare_dumps ({name = name1; results = lvh1}: result) ({name = name2; results = lvh2}: result) =
    CompareDump.compare ~name1 lvh1 ~name2 lvh2

  let count_locations (dumps: result list) =
    let module LH = Hashtbl.Make (CilType.Location) in
    let locations = LH.create 113 in
    let location_vars = RH.create 113 in
    List.iter (fun ({results; _}: result) ->
        RH.iter (fun x _ ->
            LH.replace locations (Key.to_location x) ();
            RH.replace location_vars x ()
          ) results
      ) dumps;
    (LH.length locations, RH.length location_vars)

  let main () =
    Util.init ();
    let filenames = List.tl (Array.to_list Sys.argv) in
    let dumps = List.map load filenames in
    let (locations_count, location_vars_count) = count_locations dumps in
    let i_dumps = List.mapi (fun i dump -> (i, dump)) dumps in
    List.cartesian_product i_dumps i_dumps
    (* |> List.filter (fun ((i1, _), (i2, _)) -> i1 < i2) *)
    |> List.filter (fun ((i1, _), (i2, _)) -> i1 <> i2)
    |> List.map (Tuple2.map snd snd)
    |> List.map (uncurry compare_dumps)
    |> List.iter (fun (_, msg) -> ignore (Pretty.printf "%t\n" (fun () -> msg)));
    ignore (Pretty.printf "\nTotal locations: %d\nTotal %s: %d\n" locations_count (Key.name ()) location_vars_count)
end
