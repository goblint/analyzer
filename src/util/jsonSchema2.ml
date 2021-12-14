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

let erase: type t. t Json_encoding.encoding -> unit Json_encoding.encoding = fun encoding -> Json_encoding.conv (fun _ -> failwith "erase construct") (fun _ -> ()) encoding

let rec encoding_of_schema_element (top: unit Json_encoding.encoding) (schema_element: Json_schema.element): unit Json_encoding.encoding =
  let open Json_encoding in
  match schema_element.kind with
  | Any -> unit
  | String string_specs -> erase string
  | Boolean -> erase bool
  | Integer numeric_specs -> erase int
  | Monomorphic_array (el, array_specs) ->
    erase @@ array (encoding_of_schema_element top el)
  | Id_ref "" ->
    top
  | Object object_specs ->
    List.fold_left (fun acc (name, element, required, _) ->
        let field =
          if required then
            req name (encoding_of_schema_element top element)
          else
            dft name (encoding_of_schema_element top element) ()
        in
        erase @@ merge_objs acc (obj1 field)
      ) (Option.map_default (encoding_of_schema_element top) empty object_specs.additional_properties) object_specs.properties
  | _ -> failwith (Format.asprintf "encoding_of_schema_element: %a" Json_schema.pp (Json_schema.create schema_element))

let encoding_of_schema (schema: Json_schema.schema): unit Json_encoding.encoding =
  let root = Json_schema.root schema in
  Json_encoding.mu "" (fun top -> encoding_of_schema_element top root)

open Json_encoding
open Json_schema

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

let global_schema = ref any

let validate json =
  match JE.destruct (encoding_of_schema !global_schema) json with
    | _ -> ()
    | exception (Json_encoding.Cannot_destruct _ as e) ->
      Format.printf "validate: %a\n" (Json_encoding.print_error ?print_unknown:None) e;
      failwith "JsonSchema2.validate"
