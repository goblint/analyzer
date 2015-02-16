(** Termination of loops. *)

open Batteries
open Cil
open Pretty
open Analyses

module M = Messages
let sprint f x = Pretty.sprint 80 (f () x)

module TermDomain = struct
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
  let toXML_f sf x =
    match toXML x with
      | Xml.Element (node, [text, _], elems) ->
          let summary = "Escaped Variables: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x

  let toXML s  = toXML_f short s
end

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "term"
  module D = TermDomain
  module C = TermDomain
  module G = Lattice.Unit

  let breaks : (int, location) Hashtbl.t = Hashtbl.create 13 (* break stmt sid -> corresponding loop *)
  class loopBreakVisitor = object
    inherit nopCilVisitor
    method vstmt s =
      let action s = match s.skind with
        | Loop (b, loc, Some continue, Some break) ->
            (* Printf.printf "Found loop on line %i\n" loc.line; *)
            Hashtbl.add breaks break.sid loc;
            s
        | Loop _ -> failwith "Termination.preprocess: every loop should have a break and continue stmt after prepareCFG"
        | _ -> s
      in ChangeDoChildrenPost (s, action)
  end

  let init () = visitCilFileSameGlobals (new loopBreakVisitor) !Cilfacade.ugglyImperativeHack
  let finalize () = ()

  (* queries *)
  (*let query ctx (q:Queries.t) : Queries.Result.t =*)
    (*match q with*)
      (*| Queries.MustTerm loc -> `Bool (D.mem v ctx.local)*)
      (*| _ -> Queries.Result.top ()*)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    (* if the then-block contains a goto while_break.* we have the termination condition for a loop *)
    let exits block = match block with
      | { bstmts = [{ skind = Goto (stmt, loc) }] } -> Hashtbl.find_option breaks !stmt.sid
      | _ -> None (* TODO handle return (need to find out what loop we are in) *)
    in
    match !MyCFG.current_node with
    | Some (MyCFG.Statement({ skind = If (e, tb, fb, loc) })) ->
        let b = if tv then tb else fb in
        let loop = exits b in
        if loop = None then ctx.local else ( (* current branch doesn't exit the loop *)
        M.debug_each @@ "At an If-stmt!"
          ^ "\nCil-exp: " ^ sprint d_exp e
          (*^ "; Goblint-exp: " ^ sprint d_exp exp*)
          ^ "; Goblint: " ^ sprint Queries.Result.pretty (ctx.ask (Queries.EvalInt exp))
          (*^ "\nThen-block: " ^ sprint d_block tb ^ "; Else-block: " ^ sprint d_block fb*)
          ^ "\nCurrent block is: " ^ sprint d_block b
          ^ "\nExits loop: " ^ (match loop with Some loc -> string_of_int loc.line | None -> "None")
          ;
        (*M.debug_each "Loop exit!";*)
        ctx.local)
    | _ -> ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

class loopCounterVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method vstmt s =
    let action s = match s.skind with
      | Loop (b, loc, _, _) ->
          (* insert loop counter variable *)
          let name = "term"^string_of_int loc.line in
          let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
          let v = makeLocalVar fd name ~init:(SingleInit zero) typ in
          (* make an init stmt since the init above is apparently ignored *)
          let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc) in
          (* increment it every iteration *)
          let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc) in
          b.bstmts <- inc_stmt :: b.bstmts;
          let nb = mkBlock [init_stmt; mkStmt s.skind] in
          s.skind <- Block nb;
          s
      | _ -> s
    in ChangeDoChildrenPost (s, action)
end

let _ =
  Cilfacade.register_preprocess Spec.name (new loopCounterVisitor);
  MCP.register_analysis (module Spec : Spec)
