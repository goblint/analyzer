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
    { "type"            : "string"
    , "additionalProperties" : true
    , "required"        : ["foo"]
    }
  , "gobview"         : {}
  }
}|schema}

let schema = JS.of_json (Yojson.Safe.from_string default_schema)


let rec encoding_of_schema_element (schema_element: Json_schema.element): unit Json_encoding.encoding =
  match schema_element.kind with
  | Object object_specs ->
    let open Json_encoding in
    List.fold_left (fun acc (name, element, required, _) ->
        conv (fun _ -> failwith "asd") (fun _ -> ()) @@ merge_objs acc (obj1 (if required then req name unit else dft name unit ()))
      ) (Option.map_default encoding_of_schema_element empty object_specs.additional_properties) object_specs.properties
  | _ -> failwith (Format.asprintf "%a" Json_schema.pp (Json_schema.create schema_element))

let encoding_of_schema (schema: Json_schema.schema): unit Json_encoding.encoding =
  encoding_of_schema_element (Json_schema.root schema)

let validate json =
  match JE.destruct (encoding_of_schema schema) json with
    | _ -> ()
    | exception (Json_encoding.Cannot_destruct _ as e) ->
      Json_encoding.print_error Format.std_formatter e;
      failwith "JsonSchema2.validate"