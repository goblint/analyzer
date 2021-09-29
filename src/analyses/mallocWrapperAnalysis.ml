(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses
open GobConfig

include PreMallocWrapperAnalysis

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

  let name () = "mallocWrapper"
  module D = PL
  module G = BoolDomain.MayBool
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


  type marshal = {
    heap_hash: varinfo NH.t;
    heap_vars: Node.t VH.t;
  }

  let get_heap_var node =
    (* Use existing varinfo instead of allocating a duplicate,
       which would be equal by determinism of create_var though. *)
    (* TODO: is this poor man's hashconsing? *)
    try NH.find !heap_hash node
    with Not_found ->
      let name = match node with
        | Node.Statement s -> "(alloc@sid:" ^ (string_of_int s.sid) ^ ")"
        | _ -> failwith "A function entry or return node can not be the node after a malloc" in
      let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
      NH.add !heap_hash node newvar;
      VH.add !heap_vars newvar node;
      newvar

  let query (ctx: (D.t, G.t, C.t) ctx) (type a) (q: a Q.t): a Queries.result =
    match q with
    | Q.HeapVar ->
      let node = match ctx.local with
        | `Lifted vinfo -> vinfo
        | _ -> ctx.node in
      `Lifted (get_heap_var node)
    | Q.IsHeapVar v ->
      VH.mem !heap_vars v
    | Q.IsMultiple v ->
      VH.mem !heap_vars v
    | _ -> Queries.Result.top q

  let init marshal =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (get_string_list "exp.malloc.wrappers");
    match marshal with
    | Some m ->
      heap_hash := m.heap_hash;
      heap_vars := m.heap_vars
    | None ->
      (* TODO: is this necessary? resetting between multiple analyze_loop-s/phases? *)
      NH.clear !heap_hash;
      VH.clear !heap_vars

  let finalize () =
    {heap_hash = !heap_hash; heap_vars = !heap_vars}
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
