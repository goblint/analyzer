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


  (* refs to reassign unmarshaled in init *)
  let heap_hash = ref (Hashtbl.create 113)
  let heap_vars = ref (Hashtbl.create 113)

  type marshal = {
    heap_hash: (string, varinfo) Hashtbl.t;
    heap_vars: (int, unit) Hashtbl.t;
  }

  let get_heap_var node =
    (* Use existing varinfo instead of allocating a duplicate,
       which would be equal by determinism of create_var though. *)
    (* TODO: is this poor man's hashconsing? *)
    let nodeId = match node with
      | Node.Statement s -> "sid:" ^ (string_of_int s.sid)
      | Function f -> "vid:" ^ (string_of_int f.svar.vid)
      | _ -> raise (Failure "A function entry node can never be the node after a malloc") in
    try Hashtbl.find !heap_hash nodeId
    with Not_found ->
      let name = "(alloc@" ^ nodeId ^ ")" in
      let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
      Hashtbl.add !heap_hash nodeId newvar;
      Hashtbl.add !heap_vars newvar.vid ();
      newvar

  let query (ctx: (D.t, G.t, C.t) ctx) (type a) (q: a Q.t): a Queries.result =
    match q with
    | Q.HeapVar ->
      let node = match ctx.local with
        | `Lifted vinfo -> vinfo
        | _ -> ctx.node in
      `Lifted (get_heap_var node)
    | Q.IsHeapVar v ->
      Hashtbl.mem !heap_vars v.vid
    | Q.IsMultiple v ->
      Hashtbl.mem !heap_vars v.vid
    | _ -> Queries.Result.top q

  let init marshal =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (get_string_list "exp.malloc.wrappers");
    match marshal with
    | Some m ->
      heap_hash := m.heap_hash;
      heap_vars := m.heap_vars
    | None ->
      (* TODO: is this necessary? resetting between multiple analyze_loop-s/phases? *)
      Hashtbl.clear !heap_hash;
      Hashtbl.clear !heap_vars

  let finalize () =
    {heap_hash = !heap_hash; heap_vars = !heap_vars}
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
