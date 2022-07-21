(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses
let prefix_non_definite_mem = Lval.prefix_non_definite_mem

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  module F = Lattice.Unit

  let name () = "mallocWrapperTypeBased"
  module D = F
  module G = Lattice.Unit
  module C = D

  module Q = Queries

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ()

  let branch ctx (exp:exp) (tv:bool) : D.t = ()

  let body ctx (f:fundec) : D.t = ()

  let return ctx (exp:exp option) (f:fundec) : D.t = ()

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let calleectx = () in
    [(ctx.local, calleectx)]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t = ()

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = ()

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()

  let heap_hash = Hashtbl.create 113
  let arg_hash = Hashtbl.create 113
  let heap_vars = Hashtbl.create 113
  let arg_vars = Hashtbl.create 113


  let get_heap_var (t : typ) (fn : varinfo) =
    let ts = typeSig t in
    try Hashtbl.find heap_hash (ts, fn)
    with Not_found ->
      let tsname = Pretty.sprint ~width:80 (d_typsig () ts) in
      (* "nd" for "not definite" - this does not refer to one particular memory block *)
      let name = prefix_non_definite_mem ^ "@" ^ fn.vname ^ ":" ^ tsname ^ ")" in
      let newvar = Goblintutil.create_var (makeGlobalVar name t) in
      Hashtbl.add heap_hash (ts, fn) newvar;
      Hashtbl.add heap_vars newvar.vid ();
      newvar

  let get_arg_var (t : typ) =
    let t = unrollType t in
    let ts = Cil.typeSig t in
    try Hashtbl.find arg_hash ts
    with Not_found ->
      let tsname = Pretty.sprint ~width:80 (d_typsig () ts) in
      (* "nd" for "not definite" *)
      let name = prefix_non_definite_mem ^ ":" ^ tsname ^ ")" in
      let newvar = Goblintutil.create_var (makeGlobalVar name t) in
      Hashtbl.add arg_hash ts newvar;
      Hashtbl.add arg_vars newvar.vid ();
      newvar

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Q.ArgVarTyp t ->
      `Lifted (get_arg_var t)
    | Q.HeapVar ->
      let fn = (Node.find_fundec ctx.node).svar in
      let rval =
        match ctx.node with
          | Node.Statement s -> (match s.skind with
            Instr [i] -> (match i with
              | Set (_,e,_, _) -> Some e
              | _ -> None)
            | _ -> None)
          | _ -> None
      in
      let typ = (match rval with
        | Some e -> (match typeOf e with
         | TPtr (t,_) -> t
         | t -> TVoid [])
        | _ -> TVoid [])
      in
      M.tracel "malloc" "Malloc: Got typesig %a\n" Cil.d_type typ;
      `Lifted (get_heap_var typ fn)
    | Q.IsAllocatedVar v ->
      Hashtbl.mem heap_vars v.vid
    | Q.IsHeapVar v ->
      Hashtbl.mem heap_vars v.vid || Hashtbl.mem arg_vars v.vid
    | _ -> Q.Result.top q

    let init = function
      | Some _ ->
          Hashtbl.clear heap_hash;
          Hashtbl.clear heap_vars
      | None -> ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
