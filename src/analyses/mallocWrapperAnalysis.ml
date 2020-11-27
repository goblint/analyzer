(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses
open GobConfig

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  module PL = Lattice.Flat (Basetype.ProgLines) (struct
    let top_name = "Unknown line"
    let bot_name = "Unreachable line"
  end)

  let name () = "mallocWrapper"
  module D = PL
  module G = Lattice.Unit
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

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let calleeofinterest = Hashtbl.mem wrappers f.vname in
    let calleectx = if calleeofinterest then
       if ctx.local = `Top then
        `Lifted (MyCFG.getLoc ctx.node) (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
        else ctx.local (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
      else D.top () in  (* if an uninteresting callee is called, then we forget what was called before *)
    [(ctx.local, calleectx)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx f args = D.top ()
  let threadspawn ctx f args fctx = D.bot ()
  let exitstate  v = D.top ()

  let heap_hash = Hashtbl.create 113
  let heap_vars = Hashtbl.create 113

  let get_heap_var loc =
    try Hashtbl.find heap_hash loc
    with Not_found ->
      let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
      let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
      Hashtbl.add heap_hash loc newvar;
      Hashtbl.add heap_vars newvar.vid ();
      newvar

  let query ctx (q:Q.t) : Q.Result.t =
    match q with
    | Q.HeapVar ->
      let loc = match ctx.local with
      | `Lifted vinfo -> vinfo
      | _ -> MyCFG.getLoc ctx.node in
      `Varinfo (`Lifted (get_heap_var loc))
    | Q.IsHeapVar v ->
      `Bool (Hashtbl.mem heap_vars v.vid)
    | _ -> `Top

    let init () =
      List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (get_string_list "exp.malloc.wrappers");
      Hashtbl.clear heap_hash;
      Hashtbl.clear heap_vars
end

let _ =
  MCP.register_analysis (module Spec : Spec)
