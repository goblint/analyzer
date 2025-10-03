(** CIL utilities for relational analyses. *)

open GoblintCil

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module TypeTracked =
struct
  let is_pthread_int_type = function
    | TNamed ({tname = ("pthread_t" | "pthread_key_t" | "pthread_once_t" | "pthread_spinlock_t"); _}, _) -> true (* on Linux these pthread types are integral *)
    | _ -> false

  let type_tracked typ =
    isIntegralType typ && not (is_pthread_int_type typ)
end

(** To be used in relational analyses. *)
module Tracked: Tracked =
struct
  include TypeTracked

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in relation, but just have to be handled separately *)
    let hasTrackAttribute = List.exists (fun (Attr(s,_)) -> s = "goblint_relation_track") in
    type_tracked vi.vtype && (not (GobConfig.get_bool "annotation.goblint_relation_track") || hasTrackAttribute vi.vattr)
end

(** To be used in autotuner. *)
module AutotuneTracked: Tracked =
struct
  include TypeTracked

  let isGoblintStub v = List.exists (fun (Attr(s,_)) -> s = "goblint_stub") v.vattr

  let varinfo_tracked vi = type_tracked vi.vtype && not (isGoblintStub vi) (* must not check for goblint_relation_track because the autotuner wants to add them based on this *)
end
