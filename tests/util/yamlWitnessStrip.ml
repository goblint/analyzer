open Goblint_lib
open YamlWitnessType

module StrippedEntry =
struct
  type t = {
    entry_type: EntryType.t;
  }
  [@@deriving ord]

  let strip_file_hashes {entry_type} =
    let stripped_file_hash = "$FILE_HASH" in
    let location_strip_file_hash location: Location.t =
      {location with file_hash = stripped_file_hash}
    in
    let target_strip_file_hash target: Target.t =
      {target with file_hash = stripped_file_hash}
    in
    let invariant_strip_file_hash ({invariant_type}: InvariantSet.Invariant.t): InvariantSet.Invariant.t =
      let invariant_type: InvariantSet.InvariantType.t =
        match invariant_type with
        | LocationInvariant x ->
          LocationInvariant {x with location = location_strip_file_hash x.location}
        | LoopInvariant x ->
          LoopInvariant {x with location = location_strip_file_hash x.location}
      in
      {invariant_type}
    in
    let entry_type: EntryType.t =
      match entry_type with
      | LocationInvariant x ->
        LocationInvariant {x with location = location_strip_file_hash x.location}
      | LoopInvariant x ->
        LoopInvariant {x with location = location_strip_file_hash x.location}
      | FlowInsensitiveInvariant x ->
        FlowInsensitiveInvariant x (* no location to strip *)
      | PreconditionLoopInvariant x ->
        PreconditionLoopInvariant {x with location = location_strip_file_hash x.location}
      | LoopInvariantCertificate x ->
        LoopInvariantCertificate {x with target = target_strip_file_hash x.target}
      | PreconditionLoopInvariantCertificate x ->
        PreconditionLoopInvariantCertificate {x with target = target_strip_file_hash x.target}
      | InvariantSet x ->
        InvariantSet {content = List.map invariant_strip_file_hash x.content}
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
  let stripped_yaml_entries =
    StrippedEntrySet.elements stripped_entries
    |> List.map StrippedEntry.strip_file_hashes
    |> List.rev_map StrippedEntry.to_yaml
  in

  let stripped_yaml = `A stripped_yaml_entries in
  (* to_file/to_string uses a fixed-size buffer... *)
  let stripped_yaml_str = match GobYaml.to_string' stripped_yaml with
    | Ok text -> text
    | Error (`Msg m) -> failwith ("Yaml.to_string: " ^ m)
  in
  Batteries.output_string Batteries.stdout stripped_yaml_str

let () = main ()
