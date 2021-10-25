open! Defaults (* CircInterval / Enums / ... need initialized conf *)
open! Batteries
open Prelude
open Ana
open PrivPrecCompareUtil
open PrecCompare

module VD = BaseDomain.VD

let load filename =
  let f = open_in_bin filename in
  let dump: dump = Marshal.from_channel f in
  close_in_noerr f;
  dump

module CompareDump = Make (LV) (VD) (LVH)

let compare_dumps {name = name1; lvh = lvh1} {name = name2; lvh = lvh2} =
  CompareDump.compare ~name1 ~lvh1 ~name2 ~lvh2

let count_locations dumps =
  let module LH = Hashtbl.Make (CilType.Location) in
  let locations = LH.create 113 in
  let location_vars = LVH.create 113 in
  List.iter (fun {lvh; _} ->
      LVH.iter (fun (l, x) _ ->
          LH.replace locations l ();
          LVH.replace location_vars (l, x) ()
        ) lvh
    ) dumps;
  (LH.length locations, LVH.length location_vars)

let () =
  Cil.initCIL (); (* ValueDomain.Compound.leq depends on ptrdiffType initialization *)
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
  ignore (Pretty.printf "\nTotal locations: %d\nTotal location variables: %d\n" locations_count location_vars_count)