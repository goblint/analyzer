(** YAML witness format types. *)

module Producer =
struct
  type t = {
    name: string;
    version: string;
    (* TODO: configuration *)
    command_line: string option;
    (* TODO: description *)
  }
  [@@deriving eq, ord, hash]

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
  [@@deriving eq, ord, hash]

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
  [@@deriving eq, ord, hash]

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
    file_hash: string option;
    line: int;
    column: int option;
    function_: string option;
  }
  [@@deriving eq, ord, hash]

  let to_yaml {file_name; file_hash; line; column; function_} =
    `O ([
        ("file_name", `String file_name);
      ] @ (match file_hash with
        | Some file_hash -> [
            ("file_hash", `String file_hash);
          ]
        | None ->
          []
      ) @ [
          ("line", `Float (float_of_int line));
        ] @ (match column with
        | Some column -> [
            ("column", `Float (float_of_int column));
          ]
        | None ->
          []
      ) @ (match function_ with
        | Some function_ -> [
            ("function", `String function_);
          ]
        | None ->
          []
      ))

  let of_yaml y =
    let open GobYaml in
    let+ file_name = y |> find "file_name" >>= to_string
    and+ file_hash = y |> Yaml.Util.find "file_hash" >>= option_map to_string
    and+ line = y |> find "line" >>= to_int
    and+ column = y |> Yaml.Util.find "column" >>= option_map to_int
    and+ function_ = y |> Yaml.Util.find "function" >>= option_map to_string in
    {file_name; file_hash; line; column; function_}
end

module Invariant =
struct
  type t = {
    string: string;
    type_: string;
    format: string;
  }
  [@@deriving eq, ord, hash]

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
  [@@deriving eq, ord, hash]

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
  [@@deriving eq, ord, hash]

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
  [@@deriving eq, ord, hash]

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
  [@@deriving eq, ord, hash]

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

module InvariantSet =
struct
  module LoopInvariant =
  struct
    type t = {
      location: Location.t;
      value: string;
      format: string;
    }
    [@@deriving eq, ord, hash]

    let invariant_type = "loop_invariant"

    let to_yaml' {location; value; format} =
      [
        ("location", Location.to_yaml location);
        ("value", `String value);
        ("format", `String format);
      ]

    let of_yaml y =
      let open GobYaml in
      let+ location = y |> find "location" >>= Location.of_yaml
      and+ value = y |> find "value" >>= to_string
      and+ format = y |> find "format" >>= to_string in
      {location; value; format}
  end

  module LocationInvariant =
  struct
    include LoopInvariant

    let invariant_type = "location_invariant"
  end

  (* TODO: could maybe use GADT, but adds ugly existential layer to entry type pattern matching *)
  module InvariantType =
  struct
    type t =
      | LocationInvariant of LocationInvariant.t
      | LoopInvariant of LoopInvariant.t
    [@@deriving eq, ord, hash]

    let invariant_type = function
      | LocationInvariant _ -> LocationInvariant.invariant_type
      | LoopInvariant _ -> LoopInvariant.invariant_type

    let to_yaml' = function
      | LocationInvariant x -> LocationInvariant.to_yaml' x
      | LoopInvariant x -> LoopInvariant.to_yaml' x

    let of_yaml y =
      let open GobYaml in
      let* invariant_type = y |> find "type" >>= to_string in
      if invariant_type = LocationInvariant.invariant_type then
        let+ x = y |> LocationInvariant.of_yaml in
        LocationInvariant x
      else if invariant_type = LoopInvariant.invariant_type then
        let+ x = y |> LoopInvariant.of_yaml in
        LoopInvariant x
      else
        Error (`Msg "type")
  end

  module Invariant =
  struct
    type t = {
      invariant_type: InvariantType.t;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {invariant_type} =
      `O [
        ("invariant", `O ([
             ("type", `String (InvariantType.invariant_type invariant_type));
           ] @ InvariantType.to_yaml' invariant_type)
        )
      ]

    let of_yaml y =
      let open GobYaml in
      let+ invariant_type = y |> find "invariant" >>= InvariantType.of_yaml in
      {invariant_type}
  end

  type t = {
    content: Invariant.t list;
  }
  [@@deriving eq, ord, hash]

  let entry_type = "invariant_set"

  let to_yaml' {content} =
    [("content", `A (List.map Invariant.to_yaml content))]

  let of_yaml y =
    let open GobYaml in
    let+ content = y |> find "content" >>= list >>= list_map Invariant.of_yaml in
    {content}
end

module ViolationSequence =
struct

  module Constraint =
  struct

    module Value =
    struct
      type t =
        | String of string
        | Int of int (* Why doesn't format consider ints (for switch branches) as strings here, like everywhere else? *)
      [@@deriving eq, ord, hash]

      let to_yaml = function
        | String s -> GobYaml.string s
        | Int i -> GobYaml.int i

      let of_yaml y =
        match y with
        | `String s -> Ok (String s)
        | `Float f -> Ok (Int (int_of_float f))
        | _ -> Error (`Msg "Expected a string or integer value")
    end

    type t = {
      value: Value.t;
      format: string option;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {value; format} =
      `O ([
          ("value", Value.to_yaml value);
        ] @ (match format with
          | Some format -> [
              ("format", `String format);
            ]
          | None ->
            []
        ))

    let of_yaml y =
      let open GobYaml in
      let+ value = y |> find "value" >>= Value.of_yaml
      and+ format = y |> Yaml.Util.find "format" >>= option_map to_string in
      {value; format}
  end

  module Assumption =
  struct
    type t = {
      location: Location.t;
      action: string;
      constraint_: Constraint.t;
    }
    [@@deriving eq, ord, hash]

    let waypoint_type = "assumption"

    let to_yaml' {location; action; constraint_} =
      [
        ("location", Location.to_yaml location);
        ("action", `String action);
        ("constraint", Constraint.to_yaml constraint_);
      ]

    let of_yaml y =
      let open GobYaml in
      let+ location = y |> find "location" >>= Location.of_yaml
      and+ action = y |> find "action" >>= to_string
      and+ constraint_ = y |> find "constraint" >>= Constraint.of_yaml in
      {location; action; constraint_}
  end

  module Target =
  struct
    type t = {
      location: Location.t;
      action: string;
    }
    [@@deriving eq, ord, hash]

    let waypoint_type = "target"

    let to_yaml' {location; action} =
      [
        ("location", Location.to_yaml location);
        ("action", `String action);
      ]

    let of_yaml y =
      let open GobYaml in
      let+ location = y |> find "location" >>= Location.of_yaml
      and+ action = y |> find "action" >>= to_string in
      {location; action}
  end

  module FunctionEnter =
  struct
    include Target

    let waypoint_type = "function_enter"
  end

  module FunctionReturn =
  struct
    include Assumption

    let waypoint_type = "function_return"
  end

  module Branching =
  struct
    include Assumption

    let waypoint_type = "branching"
  end

  (* TODO: could maybe use GADT, but adds ugly existential layer to entry type pattern matching *)
  module WaypointType =
  struct
    type t =
      | Assumption of Assumption.t
      | Target of Target.t
      | FunctionEnter of FunctionEnter.t
      | FunctionReturn of FunctionReturn.t
      | Branching of Branching.t
    [@@deriving eq, ord, hash]

    let waypoint_type = function
      | Assumption _ -> Assumption.waypoint_type
      | Target _ -> Target.waypoint_type
      | FunctionEnter _ -> FunctionEnter.waypoint_type
      | FunctionReturn _ -> FunctionReturn.waypoint_type
      | Branching _ -> Branching.waypoint_type

    let to_yaml' = function
      | Assumption x -> Assumption.to_yaml' x
      | Target x -> Target.to_yaml' x
      | FunctionEnter x -> FunctionEnter.to_yaml' x
      | FunctionReturn x -> FunctionReturn.to_yaml' x
      | Branching x -> Branching.to_yaml' x

    let of_yaml y =
      let open GobYaml in
      let* waypoint_type = y |> find "type" >>= to_string in
      if waypoint_type = Assumption.waypoint_type then
        let+ x = y |> Assumption.of_yaml in
        Assumption x
      else if waypoint_type = Target.waypoint_type then
        let+ x = y |> Target.of_yaml in
        Target x
      else if waypoint_type = FunctionEnter.waypoint_type then
        let+ x = y |> FunctionEnter.of_yaml in
        FunctionEnter x
      else if waypoint_type = FunctionReturn.waypoint_type then
        let+ x = y |> FunctionReturn.of_yaml in
        FunctionReturn x
      else if waypoint_type = Branching.waypoint_type then
        let+ x = y |> Branching.of_yaml in
        Branching x
      else
        Error (`Msg "type")
  end

  module Waypoint =
  struct
    type t = {
      waypoint_type: WaypointType.t;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {waypoint_type} =
      `O [
        ("waypoint", `O ([
             ("type", `String (WaypointType.waypoint_type waypoint_type));
           ] @ WaypointType.to_yaml' waypoint_type)
        )
      ]

    let of_yaml y =
      let open GobYaml in
      let+ waypoint_type = y |> find "waypoint" >>= WaypointType.of_yaml in
      {waypoint_type}
  end

  module Segment =
  struct
    type t = {
      segment: Waypoint.t list;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {segment} =
      `O [("segment", `A (List.map Waypoint.to_yaml segment))]

    let of_yaml y =
      let open GobYaml in
      let+ segment = y |> find "segment" >>= list >>= list_map Waypoint.of_yaml in
      {segment}
  end

  type t = {
    content: Segment.t list;
  }
  [@@deriving eq, ord, hash]

  let entry_type = "violation_sequence"

  let to_yaml' {content} =
    [("content", `A (List.map Segment.to_yaml content))]

  let of_yaml y =
    let open GobYaml in
    let+ content = y |> find "content" >>= list >>= list_map Segment.of_yaml in
    {content}
end

module GhostInstrumentation =
struct

  module Initial =
  struct
    type t = {
      value: string;
      format: string;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {value; format} =
      `O [
        ("value", `String value);
        ("format", `String format);
      ]

    let of_yaml y =
      let open GobYaml in
      let+ value = y |> find "value" >>= to_string
      and+ format = y |> find "format" >>= to_string in
      {value; format}
  end

  module Variable =
  struct
    type t = {
      name: string;
      scope: string;
      type_: string;
      initial: Initial.t;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {name; scope; type_; initial} =
      `O [
        ("name", `String name);
        ("scope", `String scope);
        ("type", `String type_);
        ("initial", Initial.to_yaml initial);
      ]

    let of_yaml y =
      let open GobYaml in
      let+ name = y |> find "name" >>= to_string
      and+ scope = y |> find "scope" >>= to_string
      and+ type_ = y |> find "type" >>= to_string
      and+ initial = y |> find "initial" >>= Initial.of_yaml in
      {name; scope; type_; initial}
  end

  module Update =
  struct
    type t = {
      variable: string;
      value: string;
      format: string;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {variable; value; format} =
      `O [
        ("variable", `String variable);
        ("value", `String value);
        ("format", `String format);
      ]

    let of_yaml y =
      let open GobYaml in
      let+ variable = y |> find "variable" >>= to_string
      and+ value = y |> find "value" >>= to_string
      and+ format = y |> find "format" >>= to_string in
      {variable; value; format}
  end

  module LocationUpdate =
  struct
    type t = {
      location: Location.t;
      updates: Update.t list;
    }
    [@@deriving eq, ord, hash]

    let to_yaml {location; updates} =
      `O [
        ("location", Location.to_yaml location);
        ("updates", `A (List.map Update.to_yaml updates));
      ]

    let of_yaml y =
      let open GobYaml in
      let+ location = y |> find "location" >>= Location.of_yaml
      and+ updates = y |> find "updates" >>= list >>= list_map Update.of_yaml in
      {location; updates}
  end

  type t = {
    ghost_variables: Variable.t list;
    ghost_updates: LocationUpdate.t list;
  }
  [@@deriving eq, ord, hash]

  let entry_type = "ghost_instrumentation"

  let to_yaml' {ghost_variables; ghost_updates} =
    [("content",
      `O [
        ("ghost_variables", `A (List.map Variable.to_yaml ghost_variables));
        ("ghost_updates", `A (List.map LocationUpdate.to_yaml ghost_updates));
      ])
    ]

  let of_yaml y =
    let open GobYaml in
    let* content = y |> find "content" in
    let+ ghost_variables = content |> find "ghost_variables" >>= list >>= list_map Variable.of_yaml
    and+ ghost_updates = content |> find "ghost_updates" >>= list >>= list_map LocationUpdate.of_yaml in
    {ghost_variables; ghost_updates}
end

(* TODO: could maybe use GADT, but adds ugly existential layer to entry type pattern matching *)
module EntryType =
struct
  type t =
    | LocationInvariant of LocationInvariant.t
    | LoopInvariant of LoopInvariant.t
    | FlowInsensitiveInvariant of FlowInsensitiveInvariant.t
    | PreconditionLoopInvariant of PreconditionLoopInvariant.t
    | InvariantSet of InvariantSet.t
    | ViolationSequence of ViolationSequence.t
    | GhostInstrumentation of GhostInstrumentation.t
  [@@deriving eq, ord, hash]

  let entry_type = function
    | LocationInvariant _ -> LocationInvariant.entry_type
    | LoopInvariant _ -> LoopInvariant.entry_type
    | FlowInsensitiveInvariant _ -> FlowInsensitiveInvariant.entry_type
    | PreconditionLoopInvariant _ -> PreconditionLoopInvariant.entry_type
    | InvariantSet _ -> InvariantSet.entry_type
    | ViolationSequence _ -> ViolationSequence.entry_type
    | GhostInstrumentation _ -> GhostInstrumentation.entry_type

  let to_yaml' = function
    | LocationInvariant x -> LocationInvariant.to_yaml' x
    | LoopInvariant x -> LoopInvariant.to_yaml' x
    | FlowInsensitiveInvariant x -> FlowInsensitiveInvariant.to_yaml' x
    | PreconditionLoopInvariant x -> PreconditionLoopInvariant.to_yaml' x
    | InvariantSet x -> InvariantSet.to_yaml' x
    | ViolationSequence x -> ViolationSequence.to_yaml' x
    | GhostInstrumentation x -> GhostInstrumentation.to_yaml' x

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
    else if entry_type = InvariantSet.entry_type then
      let+ x = y |> InvariantSet.of_yaml in
      InvariantSet x
    else if entry_type = ViolationSequence.entry_type then
      let+ x = y |> ViolationSequence.of_yaml in
      ViolationSequence x
    else if entry_type = GhostInstrumentation.entry_type then
      let+ x = y |> GhostInstrumentation.of_yaml in
      GhostInstrumentation x
    else
      Error (`Msg ("entry_type " ^ entry_type))
end

module Entry =
struct
  include Printable.StdLeaf

  type t = {
    entry_type: EntryType.t;
    metadata: Metadata.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0] [@hash fun _ -> 1];
  }
  [@@deriving eq, ord, hash]

  let name () = "YAML entry"

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

  let pp ppf x = Yaml.pp ppf (to_yaml x)
  include Printable.SimpleFormat (struct
      type nonrec t = t
      let pp = pp
    end)
end
