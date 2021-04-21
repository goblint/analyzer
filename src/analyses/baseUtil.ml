open Prelude.Ana
open GobConfig
module Q = Queries

let is_global (a: Q.ask) (v: varinfo): bool =
  v.vglob || ThreadEscape.has_escaped a v

let is_static (v:varinfo): bool = v.vstorage = Static

let is_always_unknown variable = variable.vstorage = Extern || Ciltools.is_volatile_tp variable.vtype

let precious_globs = ref []
let is_precious_glob v = List.mem v.vname !precious_globs

let after_config () =
  precious_globs := List.map Json.string (get_list "exp.precious_globs")

let _ =
  AfterConfig.register after_config