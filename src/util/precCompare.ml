(** Precision comparison. *)

open Batteries
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
        | {Comparison.more_precise = 0; less_precise = 0; incomparable = 0; _} -> ()
        | _ ->
          if verbose then Logs.debug "%a: %t" K.pretty k (fun () -> msg)
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
    let dump: dump = In_channel.with_open_bin filename Stdlib.Marshal.from_channel in
    let dump: result = {name = dump.name; results = unmarshal dump.marshalled } in
    dump

  module CompareDump = MakeHashtbl (Key) (Dom) (RH)

  let comparisons = ref []

  let compare_dumps ({name = name1; results = lvh1}: result) ({name = name2; results = lvh2}: result) =
    let (c, d) = CompareDump.compare ~verbose:true ~name1 lvh1 ~name2 lvh2 in
    comparisons := (name1, name2, c, d) :: !comparisons;
    (c, d)

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

  let group () =
    let new_bucket_id = ref 0 in
    let equality_buckets = Hashtbl.create 113 in
    let sorted = List.sort (fun (n1, _, _, _) (n2, _, _, _) -> String.compare n1 n2) !comparisons in
    List.iter (fun (name1, name2, (c:Comparison.t), _) ->
        (if not (Hashtbl.mem equality_buckets name1) then
           (* Make its own bucket if it does not appear yet *)
           (let bucket_id = !new_bucket_id in
            incr new_bucket_id;
            Hashtbl.add equality_buckets name1 bucket_id));
        if c.more_precise = 0 && c.less_precise = 0 && c.incomparable = 0 then
          Hashtbl.replace equality_buckets name2 (Hashtbl.find equality_buckets name1)
        else
          ()
      ) sorted;
    let bindings = Hashtbl.bindings equality_buckets in
    let buckets = List.group (fun (_, b) (_, b') -> compare b b') bindings in
    List.iter (fun bucket ->
        Logs.result "Bucket %d:" (snd (List.hd bucket));
        List.iter (fun (name, _) -> Logs.result "  %s" name) bucket
      ) buckets;
    let comparison_produced = Hashtbl.create 113 in
    List.iter (fun (name1, name2, c,d) ->
        let bucket1 = Hashtbl.find equality_buckets name1 in
        let bucket2 = Hashtbl.find equality_buckets name2 in
        if bucket1 = bucket2 then
          ()
        else
          begin
            let comp_tumple = (min bucket1 bucket2, max bucket1 bucket2) in
            if not @@ Hashtbl.mem comparison_produced comp_tumple then
              begin
                Hashtbl.add comparison_produced comp_tumple ();
                Logs.result "Comparison between bucket %d and %d: %t" (fst comp_tumple) (snd comp_tumple) (fun () -> d);
              end
          end
      ) sorted;
    ()

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
    |> List.iter (fun (_, msg) -> Logs.result "%t" (fun () -> msg));
    Logs.newline ();
    Logs.result "Total locations: %d\nTotal %s: %d" locations_count (Key.name ()) location_vars_count;
    group ()
end
