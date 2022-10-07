open Goblint_lib
open YamlWitnessType

module StrippedEntry =
struct
  type t = {
    entry_type: EntryType.t;
  }
  [@@deriving ord]

  let strip_file_hashes {entry_type} =
    let stripped_file_hash = "$STRIPPED_FILE_HASH" in
    let location_strip_file_hash location: Location.t =
      {location with file_hash = stripped_file_hash}
    in
    let target_strip_file_hash target: Target.t =
      {target with file_hash = stripped_file_hash}
    in
    let entry_type: EntryType.t =
      match entry_type with
      | LoopInvariant x ->
        LoopInvariant {x with location = location_strip_file_hash x.location}
      | PreconditionLoopInvariant x ->
        PreconditionLoopInvariant {x with location = location_strip_file_hash x.location}
      | LoopInvariantCertificate x ->
        LoopInvariantCertificate {x with target = target_strip_file_hash x.target}
      | PreconditionLoopInvariantCertificate x ->
        PreconditionLoopInvariantCertificate {x with target = target_strip_file_hash x.target}
      | _ ->
        entry_type
    in
    {entry_type}

  let to_yaml {entry_type} =
    `O ([
        ("entry_type", `String (EntryType.entry_type entry_type));
      ] @ EntryType.to_yaml' entry_type)
end

(* Use set for output, so order is deterministic regardless of Goblint. *)
module StrippedEntrySet = Set.Make (StrippedEntry)

let main () =
  let yaml_str = Batteries.input_all stdin in
  let yaml = Yaml.of_string_exn yaml_str in
  let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

  let entries' = List.fold_left (fun entries' yaml_entry ->
      match YamlWitnessType.Entry.of_yaml yaml_entry with
      | Ok {entry_type; _} ->
        let stripped_entry: StrippedEntry.t = {entry_type} in
        StrippedEntrySet.add stripped_entry entries'
      | Error (`Msg e) ->
        Format.eprintf "couldn't parse entry: %s" e;
        entries'
    ) StrippedEntrySet.empty yaml_entries
  in
  let yaml_entries' =
    StrippedEntrySet.elements entries'
    |> List.map StrippedEntry.strip_file_hashes
    |> List.map StrippedEntry.to_yaml
  in

  let yaml' = `A (List.rev yaml_entries') in
  (* to_file/to_string uses a fixed-size buffer... *)
  (* estimate how big it should be + extra in case empty *)
  let text = Yaml.to_string_exn ~len:(List.length yaml_entries * 2048 + 2048) yaml' in
  Batteries.output_string Batteries.stdout text

let () = main ()
