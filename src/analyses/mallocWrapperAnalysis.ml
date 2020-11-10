(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses
open GobConfig

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  module CT = Lattice.Flat (Basetype.ProgLines) (struct 
    let top_name = "Unknown line"
    let bot_name = "Unreachable line" 
  end)

  let name () = "mallocWrapper"
  module D = CT
  module G = Lattice.Unit
  module C = D

  module Q = Queries

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
    let interestingfunctions = get_string_list "exp.malloc.wrappers" in
    let calleofinterest = List.mem f.vname interestingfunctions in
    let callectx = if calleofinterest then
       if ctx.local = `Top then
        `Lifted (MyCFG.getLoc ctx.node) 
        else ctx.local
      else D.top () in     
    [(ctx.local, callectx)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()

  let heap_hash = Hashtbl.create 113

  let get_heap_var loc =
    try Hashtbl.find heap_hash loc
    with Not_found ->
      let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
      let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
      Hashtbl.add heap_hash loc newvar;
      newvar

  let query ctx (q:Q.t) : Q.Result.t =
    match q with
    | Q.HeapVar -> 
      let b = match ctx.local with
      | `Lifted vinfo -> vinfo
      | _ -> MyCFG.getLoc ctx.node in
      `Varinfo (`Lifted (get_heap_var b))
    | _ -> `Top

    let init () =
      Hashtbl.clear heap_hash
end

let _ =
  MCP.register_analysis (module Spec : Spec)
