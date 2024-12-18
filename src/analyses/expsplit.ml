(** Path-sensitive analysis according to values of arbitrary given expressions ([expsplit]). *)

open Batteries
open GoblintCil
open Analyses

module M = Messages

module ID = Queries.ID

module Spec : Analyses.MCPSpec =
struct
  let name () = "expsplit"

  module D = MapDomain.MapBot (Basetype.CilExp) (ID)
  include Analyses.ValueContexts(D)

  let startstate v = D.bot ()
  let exitstate = startstate

  include Analyses.DefaultSpec
  module P = IdentityP (D)

  let emit_splits man d =
    D.iter (fun e _ ->
        man.emit (UpdateExpSplit e)
      ) d;
    d

  let emit_splits_ctx man =
    emit_splits man man.local

  let assign man (lval:lval) (rval:exp) =
    emit_splits_ctx man

  let vdecl man (var:varinfo) =
    emit_splits_ctx man

  let branch man (exp:exp) (tv:bool) =
    emit_splits_ctx man

  let enter man (lval: lval option) (f:fundec) (args:exp list) =
    [man.local, man.local]

  let body man (f:fundec) =
    emit_splits_ctx man

  let return man (exp:exp option) (f:fundec) =
    emit_splits_ctx man

  let combine_env man lval fexp f args fc au f_ask =
    let d = D.join man.local au in
    emit_splits man d (* Update/preserve splits for globals in combined environment. *)

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    emit_splits_ctx man (* Update/preserve splits over assigned variable. *)

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) =
    let d = match (LibraryFunctions.find f).special arglist, f.vname with
      | _, "__goblint_split_begin" ->
        let exp = List.hd arglist in
        let ik = Cilfacade.get_ikind_exp exp in
        (* TODO: something different for pointers, currently casts pointers to ints and loses precision (other than NULL) *)
        D.add exp (ID.top_of ik) man.local (* split immediately follows *)
      | _, "__goblint_split_end" ->
        let exp = List.hd arglist in
        D.remove exp man.local
      | Setjmp { env }, _ ->
        Option.map_default (fun lval ->
            match GobConfig.get_string "ana.setjmp.split" with
            | "none" -> man.local
            | "precise" ->
              let e = Lval lval in
              let ik = Cilfacade.get_ikind_exp e in
              D.add e (ID.top_of ik) man.local
            | "coarse" ->
              let e = Lval lval in
              let e = BinOp (Eq, e, integer 0, intType) in
              D.add e (ID.top_of IInt) man.local
            | _ -> failwith "Invalid value for ana.setjmp.split"
          ) man.local lval
      | _ ->
        man.local
    in
    emit_splits man d

  let threadenter man ~multiple lval f args = [man.local]

  let threadspawn man ~multiple lval f args fman =
    emit_splits_ctx man

  let event man (event: Events.t) octx =
    match event with
    | UpdateExpSplit exp ->
      let value = man.ask (EvalInt exp) in
      D.add exp value man.local
    | Longjmped _ ->
      emit_splits_ctx man
    | _ ->
      man.local
end

let () =
  MCP.register_analysis (module Spec : MCPSpec)
