(** An analysis specification for didactic purposes. *)
(** Adapted to learn how goblint works. *)

(** Idea: Track locals and see if they are ever assigned to (intraproceduraly) *)
(** Definition of an abstract domain that would go cil.lval -> ? *)
(** going into body gives a list of all local vars of a function *)
(** when we assign and the right side is known to be assigned to, so are we *)
(** else it depends on the fact whether every var used there is assigned *)

open Prelude.Ana
open Analyses

module M = Messages

module ToyDomain = struct
  include MapDomain.MapBot_LiftTop(Basetype.Variables)
      (Lattice.Flat(Lattice.Unit)(struct let bot_name = "bot"
         let top_name = "top" end))
      (*   \top - () - \bot    *)
end

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "toy"

  module D = ToyDomain (* domain - is of the type we defined ourselves *)
  module G = Lattice.Unit (* lattice for globals (we do not care about them here) *)
  module C = ToyDomain (* context - is of the type we defined ourselves*)

  (* helper function - checks if all vars used in rval have been assigned to  *)
  (* first see if the base analysis knows sth about the variable: If it does, *)
  (* we skip the lookup in our own data structure. Might not be efficient in *)
  (* the real world but a good opportunity to learn about the query system *)
  let rec are_all_assigned ctx (rval:exp) : bool =
    match ctx.ask (Queries.EvalInt rval) with
    | `Int i -> (Messages.report "We got sth via the query system\n"; true)
    | _  -> (* whatever exp is the other analyses don't know some int that evaluates *)
      match rval with  (* to, so we do a recursive lookup for all the involved vars *)
      | Const _ -> true
      | Lval (Var v, ofs) -> (ToyDomain.find v ctx.local) = (`Lifted ())
      | Lval (Mem _, _) -> false
      | UnOp (_, e ,_) -> are_all_assigned ctx e
      | BinOp (_, e, f, _) -> are_all_assigned ctx e && are_all_assigned ctx f
      | _ -> false (* sound default, assume it's not ok *)

  let rec remove_all ctx (vs:varinfo list) =
    match vs with
    | v::t -> remove_all (D.remove v ctx) t
    | [] -> ctx

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
    | Queries.TheAnswerToLifeUniverseAndEverything -> Queries.Result.top ()
    | _ -> Queries.Result.top () (* this is the trivial query function that just says dunno to everything *)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | (Var v, ofs) when are_all_assigned ctx rval ->
      if v.vglob then (Messages.report "encountered global\n"; ctx.local) else ToyDomain.add v (`Lifted ()) ctx.local
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  (* When we enter a function we map all local vars to T since they will be uninitiazed *)
  let body ctx (f:fundec) : D.t =
    let localsToTop = ToyDomain.add_list (List.map (fun x -> (x, `Top)) f.slocals) ctx.local in
    localsToTop

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ( ToyDomain.printXml stdout ctx.local;
      remove_all ctx.local f.slocals )

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* this is for function class that we don't know anything about *)
    ctx.local

  let startstate v = (Printf.printf "start state of %s\n" v.vname ; D.bot ())
    (* starting value for the state at the beginning of the main function*)
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
