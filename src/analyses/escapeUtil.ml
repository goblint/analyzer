(** Address queries and aggregate traversal for escape tracking. *)

open GoblintCil

module M = Messages
module AD = Queries.AD
module EscapedVars = EscapeDomain.EscapedVars

let vars_of_ad ad =
  let to_extra addr set =
    match addr with
    | AD.Addr.Addr (v, _) -> EscapedVars.add v set
    | _ -> set
  in
  AD.fold to_extra ad (EscapedVars.empty ())

(** Variables reachable from [e]. [on_unknown] records callers which rely on an
    unknown address set contributing nothing. *)
let reachable ?(on_unknown=fun () -> ()) (ask: Queries.ask) (e: exp): EscapedVars.t =
  match ask.f (Queries.ReachableFrom e) with
  | ad when not (AD.is_top ad) ->
    if AD.exists (function AD.Addr.UnknownPtr -> true | _ -> false) ad then on_unknown ();
    vars_of_ad ad
  | ad ->
    if M.tracing then M.tracel "escape" "reachable %a: %a" d_exp e AD.pretty ad;
    on_unknown ();
    EscapedVars.empty ()

(** Variables [e] may point to. *)
let mpt ?(on_unknown=fun () -> ()) (ask: Queries.ask) (e: exp): EscapedVars.t =
  match ask.f (Queries.MayPointTo e) with
  | ad when not (AD.is_top ad) ->
    if AD.exists (function AD.Addr.UnknownPtr -> true | _ -> false) ad then on_unknown ();
    vars_of_ad (AD.remove UnknownPtr ad)
  | ad ->
    if M.tracing then M.tracel "escape" "mpt %a: %a" d_exp e AD.pretty ad;
    on_unknown ();
    EscapedVars.empty ()

(** Non-global variables reachable from [e].

    A struct, union or array lvalue is followed member by member.
    {!Queries.ReachableFrom} answers for an aggregate lvalue with the object the
    lvalue names rather than with what its members point to, so [holder =
    replacement] would record nothing about the memory [replacement]'s members
    point at. An array of constant length is followed element by element:
    indexing it once with a constant answers for the whole array only under the
    default [trivial] array domain, and for one partition under
    [ana.base.arrays.domain partitioned]. An array whose length is not constant
    is read through its first element's address instead, which is sound but also
    reports the object the array belongs to. *)
let rec escaping ?on_unknown (ask: Queries.ask) (e: exp): EscapedVars.t =
  let reaching e = EscapedVars.filter (fun v -> not v.vglob) (reachable ?on_unknown ask e) in
  let member lval offset = escaping ?on_unknown ask (Lval (Cil.addOffsetLval offset lval)) in
  match e, unrollType (Cilfacade.typeOf e) with
  | Lval lval, TComp (comp, _) ->
    List.fold_left (fun escaped field ->
        EscapedVars.join escaped (member lval (Field (field, NoOffset)))
      ) (EscapedVars.empty ()) comp.cfields
  | Lval lval, TArray (_, length, _) ->
    begin match Cil.lenOfArray length with
      | length ->
        let rec elements index escaped =
          if index >= length then
            escaped
          else
            elements (index + 1)
              (EscapedVars.join escaped (member lval (Index (Cil.integer index, NoOffset))))
        in
        elements 0 (EscapedVars.empty ())
      | exception Cil.LenOfArray -> reaching (StartOf lval)
    end
  | _ -> reaching e

let points_into_escaped ?on_unknown (ask: Queries.ask) ~(escaped: varinfo -> bool) (pointer: exp): bool =
  EscapedVars.exists escaped (mpt ?on_unknown ask pointer)

(** Whether a store to [target] writes into memory that [escaped] already
    describes. Whatever such a store assigns escapes the same way, so this is
    the test used by escape-tracking [assign] and [combine_assign] transfers. *)
let stores_into_escaped ?on_unknown (ask: Queries.ask) ~(escaped: varinfo -> bool) (target: lval): bool =
  points_into_escaped ?on_unknown ask ~escaped (AddrOf target)
