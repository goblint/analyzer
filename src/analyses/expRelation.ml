(** An analysis specification to answer questions about how two expressions relate to each other.   *)
(** Currently this works purely syntactically on the expressions, and only for =_{must}.            *)
(** Does not keep state, this is only formulated as an analysis to integrate well into framework    *)

open Prelude.Ana
open Analyses

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec
  module D = Lattice.Unit
  module C = Lattice.Unit

  let name () = "expRelation"

  let rec canonize (e:exp) =
    match e with
      | BinOp (MinusA, BinOp(PlusA, e1, e2, typ1), e3, typ2)  when typ1 = typ2 -> (* (e1+e2)-e3 --> (e1-e3)+e2 *)
        begin                                                                     (* where + is arithmetic +   *)
          let ce1 = canonize e1 in
          let ce2 = canonize e2 in
          let ce3 = canonize e3 in
          BinOp(PlusA, BinOp(MinusA, ce1, ce3, typ1), ce2, typ2)
        end
      | BinOp (MinusA, e1, BinOp(PlusA, e2, e3, typ1), typ2)  when typ1 = typ2 -> (* e1-(e2+e3) --> (e1-e2)-e3 *)
        begin                                                                     (* where + is arithmetic +   *)
          let ce1 = canonize e1 in
          let ce2 = canonize e2 in
          let ce3 = canonize e3 in
          BinOp(MinusA, BinOp(MinusA, ce1, ce2, typ1), ce3, typ2)
        end
      | BinOp (MinusPP, BinOp(PlusPI, e1, e2, typ1), e3, typ2) -> (*                                                  *)
        begin                                                     (*          MinusPP                     PlusA       *)
          let ce1 = canonize e1 in                                (*         /      \      =>            /     \      *)
          let ce2 = canonize e2 in                                (*     PlusPI      \              MinusPP     \     *)
          let ce3 = canonize e3 in                                (*    /   \         \            /      \      \    *)
          BinOp(PlusA, BinOp(MinusPP, ce1, ce3, typ2), ce2, typ2) (*  ptr    i     array1        ptr    array1    i   *)
        end
      | BinOp (MinusPP, BinOp(MinusPI, e1, e2, typ1), e3, typ2) -> (*                                                 *)
        begin                                                      (*          MinusPP                     MinusA     *)
          let ce1 = canonize e1 in                                 (*         /      \      =>            /     \     *)
          let ce2 = canonize e2 in                                 (*     MinusPI     \              MinusPP     \    *)
          let ce3 = canonize e3 in                                 (*    /   \         \            /      \      \   *)
          BinOp(MinusA, BinOp(MinusPP, ce1, ce3, typ2), ce2, typ2) (*  ptr    i     array1        ptr    array1    i  *)
        end
      | x -> x

  let isFloat e = Cilfacade.isFloatType (Cilfacade.typeOf e)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let lvalsEq l1 l2 = CilType.Lval.equal l1 l2 in (* == would be wrong here *)
    match q with
    | Queries.EvalInt (BinOp (Eq, e1, e2, t)) when not (isFloat e1) && Basetype.CilExp.equal (canonize e1) (canonize e2) ->
      Queries.ID.of_bool (Cilfacade.get_ikind t) true
    | Queries.EvalInt (BinOp (Lt, e1, e2, t)) when not (isFloat e1) ->
      begin
        (* Compare the cilint first in the hope that it is cheaper than the LVal comparison *)
        match e1, e2 with
        | BinOp(PlusA, Lval l1, Const(CInt(i,_,_)), _), Lval l2 when (Z.compare i Z.zero > 0 && lvalsEq l1 l2) ->
          Queries.ID.of_bool (Cilfacade.get_ikind t) false  (* c > 0 => (! x+c < x) *)
        | Lval l1, BinOp(PlusA, Lval l2, Const(CInt(i,_,_)), _) when (Z.compare i Z.zero < 0 && lvalsEq l1 l2) ->
          Queries.ID.of_bool (Cilfacade.get_ikind t) false  (* c < 0 => (! x < x+c )*)
        | BinOp(MinusA, Lval l1, Const(CInt(i,_,_)), _), Lval l2 when (Z.compare i Z.zero < 0 && lvalsEq l1 l2) ->
          Queries.ID.of_bool (Cilfacade.get_ikind t) false  (* c < 0 => (! x-c < x) *)
        | Lval l1, BinOp(MinusA, Lval l2, Const(CInt(i,_,_)), _) when (Z.compare i Z.zero > 0 && lvalsEq l1 l2) ->
          Queries.ID.of_bool (Cilfacade.get_ikind t) false  (* c > 0 => (! x < x-c) *)
        | _ ->
          Queries.ID.top ()
      end
    | Queries.EvalInt (BinOp (Eq, e1, e2, t)) when not (isFloat e1) ->
      begin
        match e1,e2 with
        | BinOp(PlusA, Lval l1, Const(CInt(i,_,_)), _), Lval l2
        | Lval l2, BinOp(PlusA, Lval l1, Const(CInt(i,_,_)), _)
        | BinOp(MinusA, Lval l1, Const(CInt(i,_,_)), _), Lval l2
        | Lval l2, BinOp(MinusA, Lval l1, Const(CInt(i,_,_)), _) when Z.compare i Z.zero <> 0 && (lvalsEq l1 l2) ->
          Queries.ID.of_bool (Cilfacade.get_ikind t) false
        | _ ->
          Queries.ID.top ()
      end
    | _ -> Queries.Result.top q


  (* below here is all the usual stuff an analysis requires, we don't do anything here *)
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
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
