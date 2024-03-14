(** ([mutexGhosts]). *)

open Analyses


module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexGhosts"

  module V =
  struct
    include Node
    let is_write_only _ = true
  end

  module Locked =
  struct
    include LockDomain.Mutexes
    let name () = "locked"
  end
  module Unlocked =
  struct
    include LockDomain.Mutexes
    let name () = "unlocked"
  end
  module G = Lattice.Prod (Locked) (Unlocked)

  let event ctx e octx =
    begin match e with
      | Events.Lock (l, _) ->
        ctx.sideg ctx.prev_node (Locked.singleton l, Unlocked.bot ())
      | Events.Unlock l ->
        ctx.sideg ctx.prev_node (Locked.bot (), Unlocked.singleton l)
      | _ -> ()
    end;
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      let (locked, unlocked) = ctx.global g in
      let loc = Node.location g in
      let location_function = (Node.find_fundec g).svar.vname in
      let location = YamlWitness.Entry.location ~location:loc ~location_function in
      let entries =
        (* TODO: do ghost_variable-s only once *)
        Locked.fold (fun l acc ->
            let variable = LockDomain.Addr.show l ^ "_locked" in (* TODO: valid C name *)
            let type_ = "int" in
            let initial = "0" in
            let entry = YamlWitness.Entry.ghost_variable ~task ~variable ~type_ ~initial in
            Queries.YS.add entry acc
          ) (Locked.union locked unlocked) (Queries.YS.empty ())
      in
      let entries =
        Locked.fold (fun l acc ->
            let variable = LockDomain.Addr.show l ^ "_locked" in (* TODO: valid C name *)
            let expression = "1" in
            let entry = YamlWitness.Entry.ghost_update ~task ~location ~variable ~expression in
            Queries.YS.add entry acc
          ) locked entries
      in
      let entries =
        Unlocked.fold (fun l acc ->
            let variable = LockDomain.Addr.show l ^ "_locked" in (* TODO: valid C name *)
            let expression = "0" in
            let entry = YamlWitness.Entry.ghost_update ~task ~location ~variable ~expression in
            Queries.YS.add entry acc
          ) unlocked entries
      in
      entries
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
