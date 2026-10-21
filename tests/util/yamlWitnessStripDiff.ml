open Goblint_lib
open YamlWitnessType
open YamlWitnessStripCommon


module InvariantKindSet = Set.Make (InvariantSet.InvariantKind)

let print_invariants invariants =
  let yaml_invariants =
    InvariantKindSet.elements invariants
    |> List.rev_map YamlWitnessType.InvariantSet.InvariantKind.to_yaml
  in

  let stripped_yaml = `A yaml_invariants in
  (* to_file/to_string uses a fixed-size buffer... *)
  let stripped_yaml_str = match GobYaml.to_string' stripped_yaml with
    | Ok text -> text
    | Error (`Msg m) -> failwith ("Yaml.to_string: " ^ m)
  in
  Batteries.output_string Batteries.stdout stripped_yaml_str


let read_stripped_entries path =
  let yaml_str = BatFile.with_file_in path BatIO.read_all in
  let yaml = Yaml.of_string_exn yaml_str in
  let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

  List.fold_left (fun stripped_entries yaml_entry ->
      match StrippedEntry.of_yaml yaml_entry with
      | Ok stripped_entry ->
        StrippedEntrySet.add stripped_entry stripped_entries
      | Error (`Msg e) ->
        Format.eprintf "couldn't parse entry: %s" e;
        stripped_entries
    ) StrippedEntrySet.empty yaml_entries

let main () =
  let left_stripped_entries = read_stripped_entries Sys.argv.(1) in
  let right_stripped_entries = read_stripped_entries Sys.argv.(2) in

  let left_invariant_sets = StrippedEntrySet.filter (function
      | {entry_type = InvariantSet _} -> true
      | _ -> false
    ) left_stripped_entries
  in
  let right_invariant_sets = StrippedEntrySet.filter (function
      | {entry_type = InvariantSet _} -> true
      | _ -> false
    ) right_stripped_entries
  in
  let (left_stripped_entries, left_invariants, right_stripped_entries, right_invariants) =
    if StrippedEntrySet.cardinal left_invariant_sets = 1 && StrippedEntrySet.cardinal right_invariant_sets = 1 then (
      match StrippedEntrySet.choose left_invariant_sets, StrippedEntrySet.choose right_invariant_sets with
      | ({entry_type = InvariantSet {content = left_content}} as left_entry), ({entry_type = InvariantSet {content = right_content}} as right_entry) ->
        let left_stripped_entries = StrippedEntrySet.remove left_entry left_stripped_entries in
        let left_invariants = InvariantKindSet.of_list left_content in
        let right_stripped_entries = StrippedEntrySet.remove right_entry right_stripped_entries in
        let right_invariants = InvariantKindSet.of_list right_content in
        (left_stripped_entries, left_invariants, right_stripped_entries, right_invariants)
      | _, _ -> assert false
    )
    else
      (left_stripped_entries, InvariantKindSet.empty, right_stripped_entries, InvariantKindSet.empty)
  in

  let left_only_stripped_entries = StrippedEntrySet.diff left_stripped_entries right_stripped_entries in
  if not (StrippedEntrySet.is_empty left_only_stripped_entries) then (
    print_endline "# Left-only entries:";
    print_stripped_entries left_only_stripped_entries;
    print_endline "---";
  );

  let left_only_invariants = InvariantKindSet.diff left_invariants right_invariants in
  if not (InvariantKindSet.is_empty left_only_invariants) then (
    print_endline "# Left-only invariants:";
    print_invariants left_only_invariants;
    print_endline "---";
  );

  let right_only_stripped_entries = StrippedEntrySet.diff right_stripped_entries left_stripped_entries in
  if not (StrippedEntrySet.is_empty right_only_stripped_entries) then (
    print_endline "# Right-only entries:";
    print_stripped_entries right_only_stripped_entries;
    print_endline "---";
  );

  let right_only_invariants = InvariantKindSet.diff right_invariants left_invariants in
  if not (InvariantKindSet.is_empty right_only_invariants) then (
    print_endline "# Right-only invariants:";
    print_invariants right_only_invariants;
    print_endline "---";
  )

let () = main ()
