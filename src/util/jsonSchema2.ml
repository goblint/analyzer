open Prelude

module JS = Json_schema.Make (Json_repr.Yojson)
module JE = Json_encoding.Make (Json_repr.Yojson)

let default_schema = {schema|
{ "id"              : "root"
, "type"            : "object"
, "required"        : ["outfile", "includes", "kernel_includes", "custom_includes", "kernel-root", "justcil", "justcfg", "printstats", "verify", "mainfun", "exitfun", "otherfun", "allglobs", "keepcpp", "tempDir", "cppflags", "kernel", "dump_globs", "result", "solver", "allfuns", "nonstatic", "colors", "g2html"]
, "additionalProperties" : false
, "properties" :
  { "ana" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "sem"               : {}
  , "incremental"       : {}
  , "trans"             : {}
  , "phases"            : {}
  , "annotation" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "exp" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "dbg" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "questions" :
    { "file"            : ""
    }
  , "outfile"         : {}
  , "includes"        : {}
  , "kernel_includes" : {}
  , "custom_includes" : {}
  , "kernel-root"     : {}
  , "justcil"         : {}
  , "justcfg"         : {}
  , "printstats"      : {}
  , "verify"          : {}
  , "mainfun"         : {}
  , "exitfun"         : {}
  , "otherfun"        : {}
  , "allglobs"        : {}
  , "keepcpp"         : {}
  , "tempDir"         :
    { "type"            : "string"
    }
  , "cppflags"        : {}
  , "kernel"          : {}
  , "dump_globs"      : {}
  , "result"          :
    { "type"            : "string"
    }
  , "solver"          : {}
  , "comparesolver"   : {}
  , "solverdiffs"     : {}
  , "allfuns"         : {}
  , "nonstatic"       : {}
  , "colors"          : {}
  , "g2html"          : {}
  , "interact"        : {}
  , "save_run"        : {}
  , "load_run"        : {}
  , "compare_runs"    : {}
  , "warn_at"         : {}
  , "warn"              :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : ["foo"]
    }
  , "gobview"         : {}
  }
}|schema}

let schema = JS.of_json (Yojson.Safe.from_string default_schema)

let erase: type t. t Json_encoding.encoding -> unit Json_encoding.encoding = fun encoding -> Json_encoding.conv (fun _ -> failwith "asd") (fun _ -> ()) encoding

let rec encoding_of_schema_element (schema_element: Json_schema.element): unit Json_encoding.encoding =
  let open Json_encoding in
  match schema_element.kind with
  | Any -> unit
  | String string_specs -> erase string
  | Object object_specs ->
    List.fold_left (fun acc (name, element, required, _) ->
        let field =
          if required then
            req name (encoding_of_schema_element element)
          else
            dft name (encoding_of_schema_element element) ()
        in
        erase @@ merge_objs acc (obj1 field)
      ) (Option.map_default encoding_of_schema_element empty object_specs.additional_properties) object_specs.properties
  | _ -> failwith (Format.asprintf "encoding_of_schema_element: %a" Json_schema.pp (Json_schema.create schema_element))

let encoding_of_schema (schema: Json_schema.schema): unit Json_encoding.encoding =
  encoding_of_schema_element (Json_schema.root schema)

let validate json =
  match JE.destruct (encoding_of_schema schema) json with
    | _ -> ()
    | exception (Json_encoding.Cannot_destruct _ as e) ->
      Format.printf "validate: %a\n" (Json_encoding.print_error ?print_unknown:None) e;
      failwith "JsonSchema2.validate"

open Json_encoding
open Json_schema

(** The ultimate convenience function for writing values. *)
let one_quote = Str.regexp "\'"
let parse_goblint_json s =
  try
    let s' = Str.global_replace one_quote "\"" s in
    let v = Yojson.Safe.from_string s' in
    v
  with Yojson.Json_error _ ->
    `String s

let rec convert_schema' (json: Yojson.Safe.t) opts (prefix: string): element * DefaultsCategory.category =
  let element' ekind =
    let (cat, (desc, def)) = List.assoc (BatString.lchop prefix) opts in
    (* let name = BatString.lchop @@ Filename.extension prefix in *)
    ({(element ekind) with title = Some (BatString.lchop prefix); description = Some desc; default = Some (Json_repr.repr_to_any (module Json_repr.Yojson) (parse_goblint_json def))}, cat)
  in
  match json with
  | `String s ->
    element' @@ String string_specs
  | `Bool b ->
    element' @@ Boolean
  | `Int i ->
    element' @@ Integer numeric_specs
  | `List xs ->
    let element_schema = match prefix with
      | ".phases" -> element (Id_ref "") (* refer to entire schema itself *)
      | _ -> element (String string_specs)
    in
    element' @@ Monomorphic_array (element_schema, array_specs)
  | `Assoc xs ->
    let cat' = ref None in
    let properties = List.map (fun (key, value) ->
        let (inner, cat) = convert_schema' value opts (prefix ^ "." ^ key) in
        cat' := Some cat;
        (key, inner, false, None)
      ) xs
    in
    let cat = Option.get !cat' in
    let el = match BatString.index_after_n '.' 2 prefix with
      | exception Not_found when prefix = ".interact" || prefix = "" -> (* isn't Std; is root *)
        element @@ Object { object_specs with properties; additional_properties = None}
      | exception Not_found ->
        {(element @@ Object { object_specs with properties; additional_properties = None}) with title = Some (DefaultsCategory.show_category cat); description = Some (DefaultsCategory.catDescription cat)}
      | _ ->
        element @@ Object { object_specs with properties; additional_properties = None}
    in
    (el, cat)
  | _ -> failwith (Format.asprintf "convert_schema': %a" Yojson.Safe.pp json)


let rec create_defaults (element: element): Yojson.Safe.t =
  match element.default with
  | Some default -> Json_repr.any_to_repr (module Json_repr.Yojson) default
  | None ->
    begin match element.kind with
      | Object object_specs ->
        `Assoc (List.map (fun (name, el, _, _) ->
            (name, create_defaults el)
          ) object_specs.properties)
      | _ ->
        Format.printf "%a\n" Json_schema.pp (create element);
        failwith "create_defaults"
    end

let rec require_all (element: element): element =
  let element_kind' = match element.kind with
    | String _
    | Boolean
    | Id_ref _
    | Integer _
    | Number _ -> element.kind
    | Monomorphic_array (el, array_specs) ->
      Monomorphic_array (require_all el, {array_specs with additional_items = Option.map require_all array_specs.additional_items})
    | Object object_specs ->
      Object {object_specs with properties = List.map (fun (name, el, req, something) -> (name, require_all el, true, something)) object_specs.properties}
    | _ ->
      Format.printf "%a\n" Json_schema.pp (create element);
      failwith "require_all"
  in
  {element with kind = element_kind'}

let convert_schema json opts =
  try
    let sch = create @@ {(fst @@ convert_schema' json opts "") with id = Some ""} in (* add id to make create defs check happy, doesn't get outputted apparently *)
    (* Format.printf "schema: %a\n" Json_schema.pp sch; *)
    (* Format.printf "schema2: %a\n" (Yojson.Safe.pretty_print ~std:true) (JS.to_json sch) *)
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "schema.json") (JS.to_json sch);
    let sch_req = create @@ {(require_all (root sch)) with id = Some ""} in
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "schema_require.json") (JS.to_json sch_req);
    let defaults = create_defaults (root sch) in
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "defaults.json") defaults
  with (Json_schema.Dangling_reference u as e) ->
    Json_schema.print_error Format.err_formatter e
