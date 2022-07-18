(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses
open GobConfig
open ThreadIdDomain

module Spec: Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

  module Chain = Lattice.Chain (struct
      let n () =
        let p = get_int "ana.malloc.unique_address_count" in
        if p < 0 then
          failwith "Option ana.malloc.unique_address_count has to be non-negative"
        else p + 1 (* Unique addresses + top address *)

      let names x = if x = (n () - 1) then "top" else Format.asprintf "%d" x

    end)

  (* Map for counting malloc node visits up to n. *)
  module MallocCounter = struct
    include MapDomain.MapBot_LiftTop(PL)(Chain)

    (* Increase counter for given node. If it does not exists yet, create it. *)
    let add_malloc counter node =
      let malloc = `Lifted node in
      let count = find malloc counter in
      if Chain.is_top count then
        counter
      else
        remove malloc counter
        |> add malloc (count + 1)
  end

  module Domain = struct
    include Lattice.Prod (MallocCounter) (PL)

    let join (counter1, node1) (counter2, node2) =
      (MallocCounter.join counter1 counter2, node1)

    let has_wrapper_node (_, wrapper_node) = not @@ PL.is_top wrapper_node

    let get_count (counter, _) node = MallocCounter.find (`Lifted node) counter

  end

  module ThreadNode = struct
    include Printable.Prod3 (ThreadIdDomain.ThreadLifted) (Node) (Chain)

    (* Description that gets appended to the varinfo-name in user output. *)
    let describe_varinfo (v: varinfo) (t, node, c) =
      let loc = UpdateCil.getLoc node in
      CilType.Location.show loc

    let name_varinfo (t, node, c) =
      Format.asprintf "(alloc@sid:%s@tid:%s(#%s))" (Node.show_id node) (ThreadLifted.show t) (Chain.show c)

  end

  module NodeVarinfoMap = RichVarinfo.BiVarinfoMap.Make(ThreadNode)
  let name () = "mallocWrapper"
    
  module D = Domain
  module C = D

  module Q = Queries

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
    let counter, wrapper_node = ctx.local in
    let new_counter =
      if Hashtbl.mem wrappers f.svar.vname then
        MallocCounter.add_malloc counter ctx.prev_node
      else counter
    in
    let new_wrapper_node =
      if Hashtbl.mem wrappers f.svar.vname then
        if not @@ D.has_wrapper_node ctx.local then
          (`Lifted ctx.prev_node) (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
        else wrapper_node (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
      else PL.top () (* if an uninteresting callee is called, then we forget what was called before *)
    in
    let caller_ctx = (new_counter, wrapper_node) in
    let callee_ctx = (new_counter, new_wrapper_node) in
    [(caller_ctx, callee_ctx)]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    D.join ctx.local au

  let special (ctx: (D.t, G.t, C.t, V.t) ctx) (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let counter, wrapper_node = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist, f.vname with
    | Malloc _, _ | Calloc _, _ | Realloc _, _ -> (MallocCounter.add_malloc counter ctx.prev_node, wrapper_node)
    | _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  type marshal = NodeVarinfoMap.marshal

  let get_heap_var = NodeVarinfoMap.to_varinfo


  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Queries.result =
    let _, wrapper_node = ctx.local in
    match q with
    | Q.HeapVar ->
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> ctx.prev_node
      in
      let var = get_heap_var (ctx.ask Q.CurrentThreadId, node, D.get_count ctx.local node) in
      var.vdecl <- UpdateCil.getLoc node; (* TODO: does this do anything bad for incremental? *)
      `Lifted var
    | Q.IsHeapVar v ->
      NodeVarinfoMap.mem_varinfo v
    | Q.IsMultiple v ->
      begin match NodeVarinfoMap.from_varinfo v with
        | Some (_, _, c) -> Chain.is_top c || not (ctx.ask Q.MustBeUniqueThread)
        | None -> false
      end
    | _ -> Queries.Result.top q

  let init marshal =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (get_string_list "ana.malloc.wrappers");
    NodeVarinfoMap.unmarshal marshal

  let finalize () =
    NodeVarinfoMap.marshal ()
end

let _ =
  MCP.register_analysis (module Spec)
