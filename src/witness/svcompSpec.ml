(** SV-COMP specification strings and files. *)

open Batteries

type t =
  | UnreachCall of string
  | NoDataRace
  | NoOverflow

let of_string s =
  let s = String.strip s in
  let regexp = Str.regexp "CHECK( init(main()), LTL(G ! \\(.*\\)) )" in
  if Str.string_match regexp s 0 then
    let global_not = Str.matched_group 1 s in
    if global_not = "data-race" then
      NoDataRace
    else if global_not = "overflow" then
      NoOverflow
    else
      let call_regex = Str.regexp "call(\\(.*\\)())" in
      if Str.string_match call_regex global_not 0 then
        let f = Str.matched_group 1 global_not in
        UnreachCall f
      else
        failwith "Svcomp.Specification.of_string: unknown global not expression"
  else
    failwith "Svcomp.Specification.of_string: unknown expression"

let of_file path =
  let s = BatFile.with_file_in path BatIO.read_all in
  of_string s

let of_option () =
  let s = GobConfig.get_string "ana.specification" in
  if Sys.file_exists s then
    of_file s
  else
    of_string s

let to_string spec =
  let global_not = match spec with
    | UnreachCall f -> "call(" ^ f ^ "())"
    | NoDataRace -> "data-race"
    | NoOverflow -> "overflow"
  in
  "CHECK( init(main()), LTL(G ! " ^ global_not ^ ") )"
