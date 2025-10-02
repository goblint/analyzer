open GoblintCil

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module Tracked: Tracked =
struct
  let is_pthread_int_type = function
    | TNamed ({tname = ("pthread_t" | "pthread_key_t" | "pthread_once_t" | "pthread_spinlock_t"); _}, _) -> true (* on Linux these pthread types are integral *)
    | _ -> false

  let type_tracked typ =
    isIntegralType typ && not (is_pthread_int_type typ)

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in relation, but just have to be handled separately *)
    let hasTrackAttribute = List.exists (fun (Attr(s,_)) -> s = "goblint_relation_track") in
    type_tracked vi.vtype && (not @@ GobConfig.get_bool "annotation.goblint_relation_track" || hasTrackAttribute vi.vattr)
end
