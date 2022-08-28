open Prelude
module Pretty = GoblintCil.Pretty
open Pretty

module Comparison =
struct
  type t = {
    equal: int;
    more_precise: int;
    less_precise: int;
    incomparable: int;
  }

  let empty = {equal = 0; more_precise = 0; less_precise = 0; incomparable = 0}
  let equal = {empty with equal = 1}
  let more_precise = {empty with more_precise = 1}
  let less_precise = {empty with less_precise = 1}
  let incomparable = {empty with incomparable = 1}

  let aggregate_same c1 c2 =
    {
      equal = c1.equal + c2.equal;
      more_precise = c1.more_precise + c2.more_precise;
      less_precise = c1.less_precise + c2.less_precise;
      incomparable = c1.incomparable + c2.incomparable;
    }

  let to_string_infix {equal; more_precise; less_precise; incomparable} =
    if incomparable > 0 || (more_precise > 0 && less_precise > 0) then (* not distinguishing two incomparabilities *)
      "incomparable to"
    else if more_precise > 0 then
      "more precise than"
    else if less_precise > 0 then
      "less precise than"
    else
      "equal to"

  let total {equal; more_precise; less_precise; incomparable} =
    equal + more_precise + less_precise + incomparable

  let to_string_counts ({equal; more_precise; less_precise; incomparable} as c) =
    Printf.sprintf "equal: %d, more precise: %d, less precise: %d, incomparable: %d, total: %d" equal more_precise less_precise incomparable (total c)
end

module Make (D: Lattice.S) =
struct

  let compare ?(verbose=false) ?(name1="left") ?(name2="right") v1 v2 =
    let c = match D.leq v1 v2, D.leq v2 v1 with
      | true, true -> Comparison.equal
      | true, false -> Comparison.more_precise
      | false, true -> Comparison.less_precise
      | false, false -> Comparison.incomparable
    in
    let diff ppf =
      ppf |>
      (if D.leq v1 v2 then nil else dprintf "diff: %a\n" D.pp_diff (v1, v2))
      ++
      (if D.leq v2 v1 then nil else dprintf "reverse diff: %a\n" D.pp_diff (v2, v1))
    in
    let msg = if verbose then Pretty.dprintf "%s %s %s\n  @[%s: %a\n%s\n%s: %a\n%t@]" name1 (Comparison.to_string_infix c) name2 name1 D.pp v1 (Comparison.to_string_infix c) name2 D.pp v2 diff
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
        | {Comparison.more_precise = 0; less_precise = 0; incomparable = 0; _} -> ()
        | _ ->
          if verbose then ignore (Pretty.printf "%a: %t\n" K.pp k msg)
      ) compared;
    let c = KH.fold (fun _ (c, _) acc -> Comparison.aggregate_same c acc) compared Comparison.empty in
    let msg = Pretty.dprintf "%s %s %s    (%s)" name1 (Comparison.to_string_infix c) name2 (Comparison.to_string_counts c) in
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
    CompareDump.compare ~verbose:true ~name1 lvh1 ~name2 lvh2

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
    |> List.iter (fun (_, msg) -> ignore (Pretty.printf "%t\n" msg));
    ignore (Pretty.printf "\nTotal locations: %d\nTotal %s: %d\n" locations_count (Key.name ()) location_vars_count)
end
