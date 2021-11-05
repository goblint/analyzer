open! Defaults (* CircInterval / Enums / ... need initialized conf *)
open! Batteries
open Prelude
open Ana
open PrecCompare

module ComparePrec (Util: PrecCompareUtil.S) =
struct
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
