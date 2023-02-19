(** An analysis that handles the case when pthread_create is called from a wrapper function all over the code. *)

(* TODO: share code with mallocWrapperAnalysis *)

open Prelude.Ana
open Analyses
open GobConfig
open ThreadIdDomain
module Q = Queries

module Spec (* : Analyses.MCPSpec *) =
struct
  include Analyses.DefaultSpec

  module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

  (* module Chain = Lattice.Chain (struct
      let n () =
        let p = get_int "ana.malloc.unique_address_count" in
        if p < 0 then
          failwith "Option ana.malloc.unique_address_count has to be non-negative"
        else p + 1 (* Unique addresses + top address *)

      let names x = if x = (n () - 1) then "top" else Format.asprintf "%d" x

    end) *)

  (* Map for counting malloc node visits up to n (of the current thread). *)
  (* module MallocCounter = struct
    include MapDomain.MapBot_LiftTop(PL)(Chain)

    (* Increase counter for given node. If it does not exists yet, create it. *)
    let add_malloc counter node =
      let malloc = `Lifted node in
      let count = find malloc counter in
      if Chain.is_top count then
        counter
      else
        remove malloc counter |> add malloc (count + 1)
  end *)

  module Node : RichVarinfo.H = struct
    include Node

    (* Description that gets appended to the varinfo-name in user output. *)
    let describe_varinfo (v: varinfo) node =
      let loc = UpdateCil.getLoc node in
      CilType.Location.show loc

    let name_varinfo node =
      Format.asprintf "(threadSpawn@sid:%s)" (Node.show_id node)

  end

  module NodeVarinfoMap = RichVarinfo.BiVarinfoMap.Make(Node)
  let name () = "threadSpawnWrapper"

  module D = PL
  module C = D

  let wrappers = Hashtbl.create 13

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let wrapper_node = ctx.local in
    let new_wrapper_node =
      if Hashtbl.mem wrappers f.svar.vname then
        match wrapper_node with
        | `Lifted _ ->  wrapper_node (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
        | _ ->  (`Lifted ctx.node) (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
      else
        PL.top () (* if an uninteresting callee is called, then we forget what was called before *)
    in
    let callee = new_wrapper_node in
    [(ctx.local, callee)]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special (ctx: (D.t, G.t, C.t, V.t) ctx) (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()

  let threadenter ctx lval f args = [D.top ()]

  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  type marshal = NodeVarinfoMap.marshal

  let get_heap_var = NodeVarinfoMap.to_varinfo


  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Q.result =
    let wrapper_node = ctx.local in
    match q with
    (* | Queries.CurrentThreadId -> wrapper_node *) (* don't really know what im doing here!! *)
    (* | Q.HeapVar ->
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> ctx.node
      in
      let count = MallocCounter.find (`Lifted node) counter in
      let var = get_heap_var (ctx.ask Q.CurrentThreadId, node, count) in
      var.vdecl <- UpdateCil.getLoc node; (* TODO: does this do anything bad for incremental? *)
      `Lifted var
    | Q.IsHeapVar v ->
      NodeVarinfoMap.mem_varinfo v
    | Q.IsMultiple v ->
      begin match NodeVarinfoMap.from_varinfo v with
        | Some (_, _, c) -> Chain.is_top c || not (ctx.ask Q.MustBeUniqueThread)
        | None -> false
      end *)
    | _ -> Queries.Result.top q

  let init marshal =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (get_string_list "ana.thread.wrappers");
    NodeVarinfoMap.unmarshal marshal

  let finalize () =
    NodeVarinfoMap.marshal ()
end

let _ =
  MCP.register_analysis (module Spec)
