open GoblintCil

include ModularUtil0

module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module VS = Batteries.Set.Make (CilType.Varinfo)

let address_to_canonical a =
  let t = Addr.get_type a in
  type_to_varinfo t

let is_canonical (v: varinfo) =
  let canonical = type_to_varinfo v.vtype in
  CilType.Varinfo.equal v canonical

(** From a set of [reachable_adresses], find all those represented by [canonical] varinfo.
    This is the basic h^{-1}. *)
let represented_by ~(canonical:varinfo) ~(reachable: AD.t) =
  let is_represented (a: Addr.t) =
    let a_c = address_to_canonical a in
    CilType.Varinfo.equal canonical a_c
  in
  let collect_represented (a: Addr.t) (acc: AD.t) =
    if is_represented a then AD.add a acc else acc
  in
  AD.fold collect_represented reachable (AD.bot ())

(** Get list of pointers to globals for all potentially used globals by function. *)
let get_callee_globals (callee_ask: Queries.ask) =
  match callee_ask.f Queries.AccessedGlobals with
  | `Top ->
    failwith @@ "Accessed globals returned `Top!"
  | `Lifted globals ->
    VS.fold (fun v acc -> mkAddrOf (Cil.var v) :: acc) globals []

module ValueDomainExtension =
struct
  open ValueDomain
  open ValueDomain.Compound

  let rec map_back (can_v: t) ~(reachable: AD.t) : t =
    let add_offset (addr: AD.t) offs =
      AD.map (fun a -> Addr.add_offset a offs) addr
    in
    match can_v with
    | `Top
    | `Int _
    | `Float _
    | `Mutex
    | `Thread _
    | `JmpBuf _
    | `Bot -> can_v
    | `Address ad ->
      begin
        let map_back (a: Addr.t) =
          match Addr.to_var_offset a with
          | Some (vi, off) when is_canonical vi ->
            let represented_no_offs = represented_by ~canonical:vi ~reachable in
            add_offset represented_no_offs off
          | _ ->
            AD.singleton a
        in
        let addrs = AD.fold (fun a acc -> AD.join acc (map_back a)) ad (AD.bot ()) in
        `Address addrs
      end
    | `Struct s ->
      `Struct (Structs.map (map_back ~reachable) s)
    | `Union u ->
      `Union (Unions.map (fun _ -> map_back ~reachable) u)
    | `Array a ->
      `Array (CArrays.map (map_back ~reachable) a)
    | `Blob _ -> failwith "Blob not yet implemented"
end
