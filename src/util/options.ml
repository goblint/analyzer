open Json_schema

let schema =
  (* JsonSchema.schema_of_yojson (Yojson.Safe.from_file "options.schema.json") *)
  JsonSchema.schema_of_yojson (Yojson.Safe.from_string [%blob "options.schema.json"])

let require_all = JsonSchema.schema_require_all schema

let defaults_additional_field = "__additional__" (* this shouldn't conflict with any actual field *)
let defaults = JsonSchema.schema_defaults ~additional_field:defaults_additional_field schema

let () =
  (* Yojson.Safe.pretty_to_channel (Stdlib.open_out "options.require-all.schema.json") (JsonSchema.schema_to_yojson require_all); *)
  (* Yojson.Safe.pretty_to_channel (Stdlib.open_out "options.defaults.json") defaults; *)
  ()

let rec element_paths (element: element): string list =
  match element.kind with
  | String _
  | Boolean
  | Integer _
  | Number _
  | Monomorphic_array _ ->
    [""]
  | Object object_specs ->
    List.concat_map (fun (name, field_element, _, _) ->
        List.map (fun path -> name ^ "." ^ path) (element_paths field_element)
      ) object_specs.properties
  | _ ->
    Logs.Format.error "%a" Json_schema.pp (create element);
    failwith "element_paths"

let schema_paths (schema: schema): string list =
  element_paths (root schema)
  |> List.map BatString.rchop (* remove trailing '.' *)

let paths = schema_paths schema

let rec element_completions (element: element): (string * string list) list =
  let default_completion () =
    match element.default with
    | Some default ->
      [("", [Yojson.Safe.to_string (Json_repr.any_to_repr (module Json_repr.Yojson) default)])]
    | None ->
      [("", [])]
  in
  match element.kind with
  | Integer _
  | Number _
  | Monomorphic_array _ ->
    default_completion ()
  | Boolean ->
    [("", ["false"; "true"])]
  | String string_specs ->
    begin match element.enum with
      | None ->
        default_completion ()
      | Some enum ->
        let cs = List.map (fun value ->
            match Json_repr.any_to_repr (module Json_repr.Yojson) value with
            | `String value -> value
            | _ -> failwith "element_completions: string_enum"
          ) enum
        in
        [("", cs)]
    end
  | Object object_specs ->
    List.concat_map (fun (name, field_element, _, _) ->
        List.map (fun (path, cs) -> (name ^ "." ^ path, cs)) (element_completions field_element)
      ) object_specs.properties
  | _ ->
    Logs.Format.error "%a" Json_schema.pp (create element);
    failwith "element_completions"

let schema_completions (schema: schema): (string * string list) list =
  element_completions (root schema)
  |> List.map (BatTuple.Tuple2.map1 BatString.rchop) (* remove trailing '.' *)

let completions = schema_completions schema

let rec pp_options ~levels ppf (element: element) =
  match element.kind with
  | String _
  | Boolean
  | Integer _
  | Number _
  | Monomorphic_array _ ->
    (* Format.fprintf ppf "%s: %s (%a)" (Option.get element.title) (Option.get element.description) (Yojson.Safe.pretty_print ~std:false) (Json_repr.any_to_repr (module Json_repr.Yojson) (Option.get element.default)) *)
    (* Yojson screws up box indentation somehow... *)
    Format.fprintf ppf "%s: %s (%s)" (Option.get element.title) (Option.get element.description) (Yojson.Safe.to_string (Json_repr.any_to_repr (module Json_repr.Yojson) (Option.get element.default)))
  | Object object_specs when levels > 0 ->
    let properties = List.filter (fun (name, field_element, _, _) ->
        match field_element.kind with
        | Object _ when levels = 1 -> false (* avoid empty lines with --options *)
        | _ -> true
      ) object_specs.properties
    in
    let pp_property ppf (name, field_element, _, _) =
      Format.fprintf ppf "%a" (pp_options ~levels:(levels - 1)) field_element
    in
    begin match element.title with
      | Some title ->
        Format.fprintf ppf "@[<v 0>%s:@,  @[<v 0>%a@]@]" title (Format.pp_print_list pp_property) properties
      | None ->
        Format.fprintf ppf "@[<v 0>%a@]" (Format.pp_print_list pp_property) properties
    end
  | Object _ ->
    ()
  | _ ->
    failwith "pp_options"

let print_options () =
  Format.printf "%a\n" (pp_options ~levels:1) (root schema) (* nosemgrep: print-not-logging *)

let print_all_options () =
  Format.printf "%a\n" (pp_options ~levels:max_int) (root schema) (* nosemgrep: print-not-logging *)
