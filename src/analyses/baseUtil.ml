open Prelude.Ana
open GobConfig
module Q = Queries

let is_global (a: Q.ask) (v: varinfo): bool =
  v.vglob || ThreadEscape.has_escaped a v

let is_static (v:varinfo): bool = v.vstorage = Static

let is_always_unknown variable = variable.vstorage = Extern || Ciltools.is_volatile_tp variable.vtype

let exclude_from_earlyglobs = ref []
let exclude_from_invalidation = ref []

let is_excluded_from_earlyglobs v = List.mem v.vname !exclude_from_earlyglobs
let is_excluded_from_invalidation v =  List.mem v.vname !exclude_from_invalidation

let after_config () =
  exclude_from_earlyglobs := get_string_list "exp.exclude_from_earlyglobs";
  exclude_from_invalidation := get_string_list "exp.exclude_from_invalidation"

let _ =
  AfterConfig.register after_config
