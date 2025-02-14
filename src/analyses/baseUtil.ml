open GoblintCil
open GobConfig
module Q = Queries

let is_global (a: Q.ask) (v: varinfo): bool =
  v.vglob || ThreadEscape.has_escaped a v

let is_static (v:varinfo): bool = v.vstorage = Static
let is_volatile variable = Ciltools.is_volatile_tp variable.vtype

let is_always_unknown variable = variable.vstorage = Extern || Ciltools.is_volatile_tp variable.vtype

let is_excluded_from_earlyglobs v = List.mem v.vname (get_string_list "exp.exclude_from_earlyglobs")
let is_excluded_from_invalidation v =  List.mem v.vname (get_string_list "exp.exclude_from_invalidation")

let is_alloc_var (a: Q.ask) (v: varinfo): bool = a.f (Queries.IsAllocVar v)
let is_not_alloc_var (a: Q.ask) (v: varinfo): bool = not (a.f (Queries.IsAllocVar v))