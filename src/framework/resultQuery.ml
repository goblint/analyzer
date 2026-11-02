(** Perform {{!Queries.t} queries} on the constraint system solution. *)

open Analyses

module Query (SpecSys: SpecSys) =
struct
  open SpecSys

  let ask_local (gh: EQSys.G.t GHT.t) (lvar:EQSys.LVar.t) local =
    (* build a man for using the query system *)
    let rec man =
      { ask    = (fun (type a) (q: a Queries.t) -> Spec.query man q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in witness context.")
      ; node   = fst lvar
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> Obj.magic (snd lvar)) (* magic is fine because Spec is top-level Control Spec *)
      ; context = (fun () -> snd lvar)
      ; edge    = MyCFG.Skip
      ; local  = local
      ; global = (fun g -> try EQSys.G.spec (GHT.find gh (EQSys.GVar.spec g)) with Not_found -> Spec.G.bot ()) (* see 29/29 on why fallback is needed *)
      ; spawn  = (fun ?(multiple=false) v d    -> failwith "Cannot \"spawn\" in witness context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in witness context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in witness context.")
      }
    in
    Spec.query man

  let ask_local_node (gh: EQSys.G.t GHT.t) (n: Node.t) local =
    (* build a man for using the query system *)
    let rec man =
      { ask    = (fun (type a) (q: a Queries.t) -> Spec.query man q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in witness context.")
      ; node   = n
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> man_failwith "No context in witness context.")
      ; context = (fun () -> man_failwith "No context in witness context.")
      ; edge    = MyCFG.Skip
      ; local  = local
      ; global = (fun g -> try EQSys.G.spec (GHT.find gh (EQSys.GVar.spec g)) with Not_found -> Spec.G.bot ()) (* TODO: how can be missing? *)
      ; spawn  = (fun ?(multiple=false) v d    -> failwith "Cannot \"spawn\" in witness context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in witness context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in witness context.")
      }
    in
    Spec.query man

  let ask_global (gh: EQSys.G.t GHT.t) =
    (* copied from Control for WarnGlobal *)
    (* build a man for using the query system *)
    let rec man =
      { ask    = (fun (type a) (q: a Queries.t) -> Spec.query man q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in query context.")
      ; node   = MyCFG.dummy_node (* TODO maybe ask should take a node (which could be used here) instead of a location *)
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> man_failwith "No context in query context.")
      ; context = (fun () -> man_failwith "No context in query context.")
      ; edge    = MyCFG.Skip
      ; local  = Spec.startstate GoblintCil.dummyFunDec.svar (* bot and top both silently raise and catch Deadcode in DeadcodeLifter *) (* TODO: is this startstate bad? *)
      ; global = (fun v -> EQSys.G.spec (try GHT.find gh (EQSys.GVar.spec v) with Not_found -> EQSys.G.bot ())) (* TODO: how can be missing? *)
      ; spawn  = (fun ?(multiple=false) v d   -> failwith "Cannot \"spawn\" in query context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in query context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"split\" in query context.")
      }
    in
    Spec.query man
end


module NH = Hashtbl.Make (Node)

module type SpecSysSol2 =
sig
  module FileCfg: MyCFG.FileCfg
  module SpecSys: SpecSys
  open SpecSys

  val gh: EQSys.G.t GHT.t
  val lh: Spec.D.t LHT.t
  (** Node index whose context joins are evaluated and memoized independently. *)
  val nh: Spec.D.t Lazy.t NH.t Lazy.t
  val local_node: Node.t -> Spec.D.t

  val ask_local: EQSys.LVar.t -> ?local:Spec.D.t -> 'a Queries.t -> 'a Queries.result
  val ask_local_node: Node.t -> ?local:Spec.D.t -> 'a Queries.t -> 'a Queries.result
  val ask_global: 'a Queries.t -> 'a Queries.result
end

module Make (FileCfg: MyCFG.FileCfg) (SpecSysSol: SpecSysSol): SpecSysSol2 with module SpecSys = SpecSysSol.SpecSys =
struct
  module FileCfg = FileCfg
  include SpecSysSol
  open SpecSys

  (* Build the node index on first use, but defer joining each node's
     contexts until that node is queried. This avoids expensive joins
     for nodes which witness generation never visits. *)
  let nh: Spec.D.t Lazy.t NH.t Lazy.t = lazy (
    let states = NH.create 113 in
    let nh = NH.create 113 in
    LHT.iter (fun (n, _) d ->
        (* Accumulate context states in reverse iteration order.
           Each replacement below captures the current list; the final entry
           for a node therefore contains all of that node's context states. *)
        let ds = d :: (try NH.find states n with Not_found -> []) in
        NH.replace states n ds;
        NH.replace nh n (lazy (
            (* Join states in the order that hash-table traversal encounters them.
               Collecting states first avoids a nested lazy chain per context. *)
            match List.rev ds with
            | first :: rest ->
              (* Restore iteration order and preserve the original left-associated
                 joins. Folding a list avoids a nested chain of lazy computations
                 whose depth would grow with the number of contexts. *)
              List.fold_left Spec.D.join first rest
            | [] -> assert false
          ))
      ) lh;
    nh
  )

  (* Compute a node's joined state on demand and cache it through Lazy.
     Nodes absent from the solution retain the previous bottom fallback. *)
  let local_node node =
    match NH.find_opt (Lazy.force nh) node with
    | Some local -> Lazy.force local
    | None -> Spec.D.bot ()

  module Query = Query (SpecSys)

  let ask_local lvar ?local q =
    let local = match local with
      | Some local -> local
      | None -> try LHT.find lh lvar with Not_found -> Spec.D.bot ()
    in
    Query.ask_local gh lvar local q
  let ask_local_node node ?local q =
    let local = match local with
      | Some local -> local
      | None -> local_node node
    in
    Query.ask_local_node gh node local q
  let ask_global q = Query.ask_global gh q
end
