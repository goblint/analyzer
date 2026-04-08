(** SV-COMP specification strings and files. *)

open Batteries

type t =
  | UnreachCall of string
  | NoDataRace
  | NoOverflow
  | Termination
  | ValidFree
  | ValidDeref
  | ValidMemtrack
  | ValidMemcleanup

type multi = t list

let of_string s =
  let s = String.strip s in
  let regexp_single = Str.regexp "CHECK( init(main()), LTL(G \\(.*\\)) )" in
  let regexp_negated = Str.regexp "CHECK( init(main()), LTL(G ! \\(.*\\)) )" in
  let regexp_finally = Str.regexp "CHECK( init(main()), LTL(F \\(.*\\)) )" in
  if Str.string_match regexp_negated s 0 then (
    match Str.matched_group 1 s with
    | "data-race" -> NoDataRace
    | "overflow" -> NoOverflow
    | global_not ->
      let call_regex = Str.regexp "call(\\(.*\\)())" in
      if Str.string_match call_regex global_not 0 then
        let f = Str.matched_group 1 global_not in
        UnreachCall f
      else
        failwith "Svcomp.Specification.of_string: unknown global not expression"
  )
  else if Str.string_match regexp_single s 0 then (
    match Str.matched_group 1 s with
    | "valid-free" -> ValidFree
    | "valid-deref" -> ValidDeref
    | "valid-memtrack" -> ValidMemtrack
    | "valid-memcleanup" -> ValidMemcleanup
    | _ -> failwith "Svcomp.Specification.of_string: unknown global expression"
  )
  else if Str.string_match regexp_finally s 0 then (
    match Str.matched_group 1 s with
    | "end" -> Termination
    | _ -> failwith "Svcomp.Specification.of_string: unknown finally expression"
  )
  else
    failwith "Svcomp.Specification.of_string: unknown expression"

let of_string s: multi =
  List.filter_map (fun line ->
      let line = String.strip line in
      if line = "" then
        None
      else
        Some (of_string line)
    ) (String.split_on_char '\n' s)

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
  let module Prop = struct
    type prop = F | G
    let string_of_prop = function
      | F -> "F"
      | G -> "G"
  end
  in
  let open Prop in
  let print_output prop spec_str is_neg =
    let prop = string_of_prop prop in
    if is_neg then
      Printf.sprintf "CHECK( init(main()), LTL(%s ! %s) )" prop spec_str
    else
      Printf.sprintf "CHECK( init(main()), LTL(%s %s) )" prop spec_str
  in
  let prop, spec_str, is_neg = match spec with
    | UnreachCall f -> G, "call(" ^ f ^ "())", true
    | NoDataRace -> G, "data-race", true
    | NoOverflow -> G, "overflow", true
    | ValidFree -> G, "valid-free", false
    | ValidDeref -> G, "valid-deref", false
    | ValidMemtrack -> G, "valid-memtrack", false
    | ValidMemcleanup -> G, "valid-memcleanup", false
    | Termination -> F, "end", false
  in
  print_output prop spec_str is_neg

let to_string spec =
  String.concat "\n" (List.map to_string spec)
