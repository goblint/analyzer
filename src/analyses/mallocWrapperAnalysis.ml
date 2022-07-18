(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses
open GobConfig

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

  module Node = struct
    include Node
    (* Description that gets appended to the varinfo-name in user output. *)
    let describe_varinfo (v: varinfo) node =
      let loc = UpdateCil.getLoc node in
      CilType.Location.show loc

    let name_varinfo node = match node with
      | Node.Statement s -> "(alloc@sid:" ^ (string_of_int s.sid) ^ ")"
      | _ -> failwith "A function entry or return node can not be the node after a malloc"
  end

  module NodeVarinfoMap = RichVarinfo.BiVarinfoMap.Make(Node)
  let name () = "mallocWrapper"
  module D = PL
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
    let calleeofinterest = Hashtbl.mem wrappers f.svar.vname in
    let calleectx = if calleeofinterest then
        if ctx.local = `Top then
          `Lifted ctx.node (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
        else ctx.local (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
      else D.top () in  (* if an uninteresting callee is called, then we forget what was called before *)
    [(ctx.local, calleectx)]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  type marshal = NodeVarinfoMap.marshal

  let get_heap_var = NodeVarinfoMap.to_varinfo
  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Queries.result =
    match q with
    | Q.HeapVar ->
      let node = match ctx.local with
        | `Lifted vinfo -> vinfo
        | _ -> ctx.node
      in
      let var = get_heap_var node in
      var.vdecl <- UpdateCil.getLoc node; (* TODO: does this do anything bad for incremental? *)
      `Lifted var
    | Q.IsHeapVar v ->
      NodeVarinfoMap.mem_varinfo v
    | Q.IsMultiple v ->
      NodeVarinfoMap.mem_varinfo v
    | _ -> Queries.Result.top q

  let init marshal =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (get_string_list "ana.malloc.wrappers");
    NodeVarinfoMap.unmarshal marshal

  let finalize () =
    NodeVarinfoMap.marshal ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
