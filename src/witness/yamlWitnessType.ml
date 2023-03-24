module Producer =
struct
  type t = {
    name: string;
    version: string;
    (* TODO: configuration *)
    command_line: string option;
    (* TODO: description *)
  }

  let to_yaml {name; version; command_line} =
    `O ([
        ("name", `String name);
        ("version", `String version);
      ] @ match command_line with
      | Some command_line -> [
          ("command_line", `String command_line);
        ]
      | None ->
        []
      )

  let of_yaml y =
    let open GobYaml in
    let+ name = y |> find "name" >>= to_string
    and+ version = y |> find "version" >>= to_string
    and+ command_line = y |> Yaml.Util.find "command_line" >>= option_map to_string in
    {name; version; command_line}
end

module Task =
struct
  type t = {
    input_files: string list;
    input_file_hashes: (string * string) list;
    data_model: string;
    language: string;
    specification: string option;
  }

  let to_yaml {input_files; input_file_hashes; data_model; language; specification} =
    `O ([
        ("input_files", `A (List.map Yaml.Util.string input_files));
        ("input_file_hashes", `O (List.map (fun (file, hash) ->
             (file, `String hash)
           ) input_file_hashes));
        ("data_model", `String data_model);
        ("language", `String language);
      ] @ match specification with
      | Some specification -> [
          ("specification", `String specification)
        ]
      | None ->
        []
      )

  let of_yaml y =
    let open GobYaml in
    let+ input_files = y |> find "input_files" >>= list >>= list_map to_string
    and+ input_file_hashes = y |> find "input_file_hashes" >>= entries >>= list_map (fun (file, y_hash) ->
        let+ hash = to_string y_hash in
        (file, hash)
      )
    and+ data_model = y |> find "data_model" >>= to_string
    and+ language = y |> find "language" >>= to_string
    and+ specification = y |> Yaml.Util.find "specification" >>= option_map to_string in
    {input_files; input_file_hashes; data_model; language; specification}
end

module Metadata =
struct
  type t = {
    format_version: string;
    uuid: string;
    creation_time: string;
    producer: Producer.t;
    task: Task.t option;
  }

  let to_yaml {format_version; uuid; creation_time; producer; task} =
    `O ([
        ("format_version", `String format_version);
        ("uuid", `String uuid);
        ("creation_time", `String creation_time);
        ("producer", Producer.to_yaml producer);
      ] @ match task with
      | Some task -> [
          ("task", Task.to_yaml task)
        ]
      | None ->
        []
      )
  let of_yaml y =
    let open GobYaml in
    let+ format_version = y |> find "format_version" >>= to_string
    and+ uuid = y |> find "uuid" >>= to_string
    and+ creation_time = y |> find "creation_time" >>= to_string
    and+ producer = y |> find "producer" >>= Producer.of_yaml
    and+ task = y |> Yaml.Util.find "task" >>= option_map Task.of_yaml in
    {format_version; uuid; creation_time; producer; task}
end

module Location =
struct
  type t = {
    file_name: string;
    file_hash: string;
    line: int;
    column: int;
    function_: string;
  }

  let to_yaml {file_name; file_hash; line; column; function_} =
    `O [
      ("file_name", `String file_name);
      ("file_hash", `String file_hash);
      ("line", `Float (float_of_int line));
      ("column", `Float (float_of_int column));
      ("function", `String function_);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ file_name = y |> find "file_name" >>= to_string
    and+ file_hash = y |> find "file_hash" >>= to_string
    and+ line = y |> find "line" >>= to_int
    and+ column = y |> find "column" >>= to_int
    and+ function_ = y |> find "function" >>= to_string in
    {file_name; file_hash; line; column; function_}
end

module Invariant =
struct
  type t = {
    string: string;
    type_: string;
    format: string;
  }

  let to_yaml {string; type_; format} =
    `O [
      ("string", `String string);
      ("type", `String type_);
      ("format", `String format);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ string = y |> find "string" >>= to_string
    and+ type_ = y |> find "type" >>= to_string
    and+ format = y |> find "format" >>= to_string in
    {string; type_; format}
end

module LoopInvariant =
struct
  type t = {
    location: Location.t;
    loop_invariant: Invariant.t;
  }

  let entry_type = "loop_invariant"

  let to_yaml' {location; loop_invariant} =
    [
      ("location", Location.to_yaml location);
      ("loop_invariant", Invariant.to_yaml loop_invariant);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ location = y |> find "location" >>= Location.of_yaml
    and+ loop_invariant = y |> find "loop_invariant" >>= Invariant.of_yaml in
    {location; loop_invariant}
end

module LocationInvariant =
struct
  type t = {
    location: Location.t;
    location_invariant: Invariant.t;
  }

  let entry_type = "location_invariant"

  let to_yaml' {location; location_invariant} =
    [
      ("location", Location.to_yaml location);
      ("location_invariant", Invariant.to_yaml location_invariant);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ location = y |> find "location" >>= Location.of_yaml
    and+ location_invariant = y |> find "location_invariant" >>= Invariant.of_yaml in
    {location; location_invariant}
end

module FlowInsensitiveInvariant =
struct
  type t = {
    flow_insensitive_invariant: Invariant.t;
  }

  let entry_type = "flow_insensitive_invariant"

  let to_yaml' {flow_insensitive_invariant} =
    [
      ("flow_insensitive_invariant", Invariant.to_yaml flow_insensitive_invariant);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ flow_insensitive_invariant = y |> find "flow_insensitive_invariant" >>= Invariant.of_yaml in
    {flow_insensitive_invariant}
end

module PreconditionLoopInvariant =
struct
  type t = {
    location: Location.t;
    loop_invariant: Invariant.t;
    precondition: Invariant.t;
  }

  let entry_type = "precondition_loop_invariant"

  let to_yaml' {location; loop_invariant; precondition} =
    [
      ("location", Location.to_yaml location);
      ("loop_invariant", Invariant.to_yaml loop_invariant);
      ("precondition", Invariant.to_yaml precondition);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ location = y |> find "location" >>= Location.of_yaml
    and+ loop_invariant = y |> find "loop_invariant" >>= Invariant.of_yaml
    and+ precondition = y |> find "precondition" >>= Invariant.of_yaml in
    {location; loop_invariant; precondition}
end

module Target =
struct
  type t = {
    uuid: string;
    type_: string;
    file_hash: string;
  }

  let to_yaml {uuid; type_; file_hash} =
    `O [
      ("uuid", `String uuid);
      ("type", `String type_);
      ("file_hash", `String file_hash);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ uuid = y |> find "uuid" >>= to_string
    and+ type_ = y |> find "type" >>= to_string
    and+ file_hash = y |> find "file_hash" >>= to_string in
    {uuid; type_; file_hash}
end

module Certification =
struct
  type t = {
    string: string;
    type_: string;
    format: string;
  }

  let to_yaml {string; type_; format} =
    `O [
      ("string", `String string);
      ("type", `String type_);
      ("format", `String format);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ string = y |> find "string" >>= to_string
    and+ type_ = y |> find "type" >>= to_string
    and+ format = y |> find "format" >>= to_string in
    {string; type_; format}
end

module LoopInvariantCertificate =
struct
  type t = {
    target: Target.t;
    certification: Certification.t;
  }

  let entry_type = "loop_invariant_certificate"

  let to_yaml' {target; certification} =
    [
      ("target", Target.to_yaml target);
      ("certification", Certification.to_yaml certification);
    ]

  let of_yaml y =
    let open GobYaml in
    let+ target = y |> find "target" >>= Target.of_yaml
    and+ certification = y |> find "certification" >>= Certification.of_yaml in
    {target; certification}
end

module PreconditionLoopInvariantCertificate =
struct
  include LoopInvariantCertificate
  let entry_type = "precondition_loop_invariant_certificate"
end

(* TODO: could maybe use GADT, but adds ugly existential layer to entry type pattern matching *)
module EntryType =
struct
  type t =
    | LocationInvariant of LocationInvariant.t
    | LoopInvariant of LoopInvariant.t
    | FlowInsensitiveInvariant of FlowInsensitiveInvariant.t
    | PreconditionLoopInvariant of PreconditionLoopInvariant.t
    | LoopInvariantCertificate of LoopInvariantCertificate.t
    | PreconditionLoopInvariantCertificate of PreconditionLoopInvariantCertificate.t

  let entry_type = function
    | LocationInvariant _ -> LocationInvariant.entry_type
    | LoopInvariant _ -> LoopInvariant.entry_type
    | FlowInsensitiveInvariant _ -> FlowInsensitiveInvariant.entry_type
    | PreconditionLoopInvariant _ -> PreconditionLoopInvariant.entry_type
    | LoopInvariantCertificate _ -> LoopInvariantCertificate.entry_type
    | PreconditionLoopInvariantCertificate _ -> PreconditionLoopInvariantCertificate.entry_type

  let to_yaml' = function
    | LocationInvariant x -> LocationInvariant.to_yaml' x
    | LoopInvariant x -> LoopInvariant.to_yaml' x
    | FlowInsensitiveInvariant x -> FlowInsensitiveInvariant.to_yaml' x
    | PreconditionLoopInvariant x -> PreconditionLoopInvariant.to_yaml' x
    | LoopInvariantCertificate x -> LoopInvariantCertificate.to_yaml' x
    | PreconditionLoopInvariantCertificate x -> PreconditionLoopInvariantCertificate.to_yaml' x

  let of_yaml y =
    let open GobYaml in
    let* entry_type = y |> find "entry_type" >>= to_string in
    if entry_type = LocationInvariant.entry_type then
      let+ x = y |> LocationInvariant.of_yaml in
      LocationInvariant x
    else if entry_type = LoopInvariant.entry_type then
      let+ x = y |> LoopInvariant.of_yaml in
      LoopInvariant x
    else if entry_type = FlowInsensitiveInvariant.entry_type then
      let+ x = y |> FlowInsensitiveInvariant.of_yaml in
      FlowInsensitiveInvariant x
    else if entry_type = PreconditionLoopInvariant.entry_type then
      let+ x = y |> PreconditionLoopInvariant.of_yaml in
      PreconditionLoopInvariant x
    else if entry_type = LoopInvariantCertificate.entry_type then
      let+ x = y |> LoopInvariantCertificate.of_yaml in
      LoopInvariantCertificate x
    else if entry_type = PreconditionLoopInvariantCertificate.entry_type then
      let+ x = y |> PreconditionLoopInvariantCertificate.of_yaml in
      PreconditionLoopInvariantCertificate x
    else
      Error (`Msg "entry_type")
end

module Entry =
struct
  type t = {
    entry_type: EntryType.t;
    metadata: Metadata.t;
  }

  let to_yaml {entry_type; metadata} =
    `O ([
        ("entry_type", `String (EntryType.entry_type entry_type));
        ("metadata", Metadata.to_yaml metadata);
      ] @ EntryType.to_yaml' entry_type)

  let of_yaml y =
    let open GobYaml in
    let+ metadata = y |> find "metadata" >>= Metadata.of_yaml
    and+ entry_type = y |> EntryType.of_yaml in
    {entry_type; metadata}
end
