open! Defaults (* CircInterval / Enums / ... need initialized conf *)
open! Batteries
open Prelude
open Ana
open ApronPrivPrecCompareUtil
open PrecCompare

let load filename =
  let f = open_in_bin filename in
  let m = Marshal.input f in
  let m = LVH.map (fun x -> D2.unmarshal) m in
  close_in_noerr f;
  {name = filename; lvh = m}

module CompareDump = MakeHashtbl (LV) (D2) (LVH)

let compare_dumps {name = name1; lvh = lvh1} {name = name2; lvh = lvh2} =
  CompareDump.compare ~name1 lvh1 ~name2 lvh2

let count_locations dumps =
  let module LH = Hashtbl.Make (Node) in
  let nodes = LH.create 113 in
  let location_vars = LVH.create 113 in
  List.iter (fun {lvh; _} ->
      LVH.iter (fun n _ ->
          LH.replace nodes n ();
          LVH.replace location_vars n ()
        ) lvh
    ) dumps;
  (LH.length nodes, LVH.length location_vars)

let () =
  Cil.initCIL (); (* ValueDomain.Compound.leq depends on ptrdiffType initialization *)
  ApronPrivPrecCompareUtil.init ();
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
