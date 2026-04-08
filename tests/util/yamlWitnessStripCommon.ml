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
    let location_strip_file_hash (location: Location.t): Location.t =
      let file_hash =
        match location.file_hash with
        | Some _ -> Some stripped_file_hash (* TODO: or just map to None always? *)
        | None -> None
      in
      {location with file_hash}
    in
    let invariant_strip_file_hash ({invariant_type}: InvariantSet.Invariant.t): InvariantSet.Invariant.t =
      let invariant_type: InvariantSet.InvariantType.t =
        match invariant_type with
        | LocationInvariant x ->
          LocationInvariant {x with location = location_strip_file_hash x.location}
        | LoopInvariant x ->
          LoopInvariant {x with location = location_strip_file_hash x.location}
        | LoopTransitionInvariant x ->
          LoopTransitionInvariant {x with location = location_strip_file_hash x.location}
        | LocationTransitionInvariant x ->
          LocationTransitionInvariant {x with location = location_strip_file_hash x.location}
        | FlowInsensitiveInvariant x ->
          FlowInsensitiveInvariant x (* no location to strip *)
      in
      {invariant_type}
    in
    let contract_strip_file_hash ({contract_type}: InvariantSet.Contract.t): InvariantSet.Contract.t =
      let contract_type: InvariantSet.ContractType.t =
        match contract_type with
        | FunctionContract x ->
          FunctionContract {x with location = location_strip_file_hash x.location}
      in
      {contract_type}
    in
    let invariant_kind_strip_file_hash (invariant_kind: InvariantSet.InvariantKind.t): InvariantSet.InvariantKind.t =
      match invariant_kind with
      | Invariant x ->
        Invariant (invariant_strip_file_hash x)
      | Contract x ->
        Contract (contract_strip_file_hash x)
    in
    let waypoint_strip_file_hash ({waypoint_type}: ViolationSequence.Waypoint.t): ViolationSequence.Waypoint.t =
      let waypoint_type: ViolationSequence.WaypointType.t =
        match waypoint_type with
        | Assumption x ->
          Assumption {x with location = location_strip_file_hash x.location}
        | Target x ->
          Target {x with location = location_strip_file_hash x.location}
        | FunctionEnter x ->
          FunctionEnter {x with location = location_strip_file_hash x.location}
        | FunctionReturn x ->
          FunctionReturn {x with location = location_strip_file_hash x.location}
        | Branching x ->
          Branching {x with location = location_strip_file_hash x.location}
      in
      {waypoint_type}
    in
    let segment_strip_file_hash ({segment}: ViolationSequence.Segment.t): ViolationSequence.Segment.t =
      {segment = List.map waypoint_strip_file_hash segment}
    in
    let ghost_location_update_strip_file_hash (x: GhostInstrumentation.LocationUpdate.t): GhostInstrumentation.LocationUpdate.t =
      {
        location = location_strip_file_hash x.location;
        updates = List.sort GhostInstrumentation.Update.compare x.updates
      }
    in
    let entry_type: EntryType.t =
      match entry_type with
      | InvariantSet x ->
        InvariantSet {content = List.sort InvariantSet.InvariantKind.compare (List.map invariant_kind_strip_file_hash x.content)} (* Sort, so order is deterministic regardless of Goblint. *)
      | ViolationSequence x ->
        ViolationSequence {content = List.map segment_strip_file_hash x.content}
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
