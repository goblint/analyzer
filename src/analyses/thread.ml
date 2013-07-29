(** Thread-id analyses. *)
open Cil
open Pretty
open Analyses

module T  = ConcDomain.Thread
module TS = ConcDomain.ThreadSet

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "thread"
  module D = ConcDomain.ThreadSet
  module C = D
  module G = ConcDomain.ThreadCreation
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t = ctx.local
    
  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)
    
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a)
                     && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) -> 
          Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []    
      | _ -> []
  
  let get_current_tid ctx =
    snd (snd (Obj.obj (List.assoc "base" ctx.presub)))

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match LibraryFunctions.classify f.vname arglist with
      | `ThreadCreate (fn, x) -> 
          let fns = eval_exp_addr ctx.ask fn in
          let threads = List.concat (List.map ValueDomain.Addr.to_var_may fns) in
          let l = !Tracing.current_loc in
          let creator = get_current_tid ctx in
          let new_thread x = 
            let tid = T.spawn_thread l x in
            let repeated = D.mem tid ctx.local in
            let eff = 
              match creator with
                | `Lifted ctid -> (repeated, TS.singleton ctid)
                | `Top         -> (true,     TS.bot ())
                | `Bot         -> (false,    TS.bot ())
            in
            ctx.sideg tid eff; tid
          in
          let add_thread s x = D.add (new_thread x) s in
          List.fold_left add_thread ctx.local threads
      | _ -> ctx.local

  let query ctx (q: Queries.t) = 
    match q with
      | Queries.IsNotUnique -> begin
          let rec check_one tid = 
            let (rep, parents) = ctx.global tid in
            let n = TS.cardinal parents in
              (* A thread is not unique if it is 
               * a) repeatedly created, 
               * b) created in multiple threads, or 
               * c) created by a thread that is itself multiply created.
               * Note that starting threads have empty ancestor sets! *)
              rep || n > 1 || n > 0 && check_one (TS.choose parents)
          in
          let tid = get_current_tid ctx in
          match tid with
            | `Lifted tid -> `Bool (check_one tid)
            | _ -> `Bool (true)
        end
      | _ -> Queries.Result.top ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

(* really stupid thread-ids *)
module StartLocIDs =
struct
  include Analyses.DefaultSpec

  let name = "thread-id-location"
  module D = ConcDomain.ThreadStringSet
  module C = D
  module G = Lattice.Unit
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t = ctx.local
    
  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)
    
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a)
                     && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) -> 
          Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []    
      | _ -> []
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    begin match LibraryFunctions.classify f.vname arglist with
      | `ThreadCreate (fn, x) -> 
          let fns = eval_exp_addr ctx.ask fn in
          let location x = let l = !Tracing.current_loc in l.file ^ ":" ^ string_of_int l.line ^ ":" ^ x.vname in
          let new_thread x = ctx.spawn x (D.singleton (location x)) in
          List.iter new_thread (List.concat (List.map ValueDomain.Addr.to_var_may fns))
      | _ -> ()
    end;
    ctx.local

  let main = D.singleton "main"
  let startstate v = main
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ = MCP.register_analysis (module StartLocIDs : Spec)
let _ = MCP.register_analysis ~dep:["base"] (module Spec : Spec)
