open YamlWitnessStripCommon

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
  let left_only_stripped_entries = StrippedEntrySet.diff left_stripped_entries right_stripped_entries in
  let right_only_stripped_entries = StrippedEntrySet.diff right_stripped_entries left_stripped_entries in

  print_endline "# Left-only entries:";
  print_stripped_entries left_only_stripped_entries;
  print_endline "---";
  print_endline "# Right-only entries:";
  print_stripped_entries right_only_stripped_entries

let () = main ()
