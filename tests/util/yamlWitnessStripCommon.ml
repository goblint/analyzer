open Goblint_lib
open YamlWitnessType

module StrippedEntry =
struct
  type t = {
    entry_type: EntryType.t;
  }
  [@@deriving ord]

  let strip_file_hashes {entry_type} =
    let ghost_location_update_strip_file_hash (x: GhostInstrumentation.LocationUpdate.t): GhostInstrumentation.LocationUpdate.t =
      {
        x with
        updates = List.sort GhostInstrumentation.Update.compare x.updates (* Sort, so order is deterministic regardless of Goblint. *)
      }
    in
    let entry_type: EntryType.t =
      match entry_type with
      | InvariantSet x ->
        InvariantSet {content = List.sort InvariantSet.InvariantKind.compare x.content} (* Sort, so order is deterministic regardless of Goblint. *)
      | ViolationSequence x ->
        ViolationSequence x (* Order of segments is important. *) (* TODO: Sort waypoints in segment? *)
      | GhostInstrumentation x ->
        GhostInstrumentation { (* Sort, so order is deterministic regardless of Goblint. *)
          ghost_variables = List.sort GhostInstrumentation.Variable.compare x.ghost_variables;
          ghost_updates = List.sort GhostInstrumentation.LocationUpdate.compare (List.map ghost_location_update_strip_file_hash x.ghost_updates);
        }
    in
    {entry_type}

  let to_yaml {entry_type} =
    `O ([
        ("entry_type", `String (EntryType.entry_type entry_type));
      ] @ EntryType.to_yaml' entry_type)

  let of_yaml y =
    let open GobYaml in
    let+ entry_type = y |> EntryType.of_yaml in
    {entry_type}
end

(* Use set for output, so order is deterministic regardless of Goblint. *)
module StrippedEntrySet = Set.Make (StrippedEntry)

let print_stripped_entries stripped_entries =
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
