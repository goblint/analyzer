open Goblint_lib
open YamlWitnessStripCommon

(* Use set for output, so order is deterministic regardless of Goblint. *)
module StrippedEntrySet = Set.Make (StrippedEntry)

let main () =
  let yaml_str = Batteries.input_all stdin in
  let yaml = Yaml.of_string_exn yaml_str in
  let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

  let stripped_entries = List.fold_left (fun stripped_entries yaml_entry ->
      match YamlWitnessType.Entry.of_yaml yaml_entry with
      | Ok {entry_type; _} ->
        let stripped_entry: StrippedEntry.t = {entry_type} in
        StrippedEntrySet.add stripped_entry stripped_entries
      | Error (`Msg e) ->
        Format.eprintf "couldn't parse entry: %s" e;
        stripped_entries
    ) StrippedEntrySet.empty yaml_entries
  in

  print_stripped_entries stripped_entries

let () = main ()
