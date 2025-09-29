(** Tool for converting YAML witness 0.1 ([location_invariant] and [loop_invariant] entries) to YAML witness 2.0 ([invariant_set] entry). *)

open Goblint_lib
open YamlWitnessType

let invariant2invariant_set_invariant ~location (invariant: Invariant.t): InvariantSet.LoopInvariant.t =
  assert (invariant.type_ = "assertion");
  assert (invariant.format = "C");
  {
    location;
    value = invariant.string;
    format = "c_expression"
  }

let strip_location (location: Location.t): Location.t =
  {location with file_hash = None}

let main () =
  let yaml_str = Batteries.input_all stdin in
  let yaml = Yaml.of_string_exn yaml_str in
  let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

  let metadata': Metadata.t option ref = ref None in
  let invariants = List.map (fun yaml_entry ->
      let entry = Entry.of_yaml yaml_entry |> Result.get_ok in
      begin match !metadata' with
        | None ->
          metadata' := Some entry.metadata
        | Some metadata ->
          (* all existing metadata should be the same *)
          (* they (should) have different UUIDs, so change UUID to match to compare the rest *)
          assert (Metadata.equal metadata {entry.metadata with uuid = metadata.uuid})
      end;
      match entry.entry_type with
      | LocationInvariant {location; location_invariant} ->
        let location = strip_location location in
        let invariant = invariant2invariant_set_invariant ~location location_invariant in
        {InvariantSet.Invariant.invariant_type = LocationInvariant invariant}
      | LoopInvariant {location; loop_invariant} ->
        let location = strip_location location in
        let invariant = invariant2invariant_set_invariant ~location loop_invariant in
        {InvariantSet.Invariant.invariant_type = LoopInvariant invariant}
      | _ -> failwith "unsupported entry type"
    ) yaml_entries
  in

  let invariant_set: InvariantSet.t = {content = invariants} in
  let entry': Entry.t = {
    metadata = {(Option.get !metadata') with format_version = "2.0"};
    entry_type = InvariantSet invariant_set
  }
  in

  let yaml' = `A [Entry.to_yaml entry'] in
  (* to_file/to_string uses a fixed-size buffer... *)
  let yaml_str' = match GobYaml.to_string' yaml' with
    | Ok text -> text
    | Error (`Msg m) -> failwith ("Yaml.to_string: " ^ m)
  in
  Batteries.output_string Batteries.stdout yaml_str'

let () = main ()
