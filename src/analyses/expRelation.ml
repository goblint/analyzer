(** An analysis specification to answer questions about how two expressions relate to each other. *)
(** Currently this works purely statically on the expressions, but at a later stage, this analysis *)
(** can be improved to perform more involved things *)
open Prelude.Ana
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec
  module D = Lattice.Unit
  module G = Lattice.Unit
  module C = Lattice.Unit

  let name = "expRelation"

  let rec canonize (e:exp) =
    match e with
      | BinOp (MinusA, BinOp(PlusA, e1, e2, typ1), e3, typ2)  when typ1 = typ2 -> (* (e1+e2)-e3 --> (e1-e3)+e2 *)
        begin                                                                      (* where + is arithmetic +   *)
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
      | BinOp (MinusPP, BinOp(PlusPI, e1, e2, typ1), e3, typ2) -> (*                                                    *)
        begin                                                     (*          MinusPP                     PlusA         *)
          let ce1 = canonize e1 in                                (*         /      \      =>            /     \        *)
          let ce2 = canonize e2 in                                (*     PlusPI      \              MinusPP     \       *)
          let ce3 = canonize e3 in                                (*    /   \         \            /      \      \      *) 
          BinOp(PlusA, BinOp(MinusPP, ce1, ce3, typ2), ce2, typ2) (*  ptr    i     array1        ptr    array1    i     *)
        end
      | BinOp (MinusPP, BinOp(MinusPI, e1, e2, typ1), e3, typ2) -> (*                                                    *)
        begin                                                      (*          MinusPP                     MinusA        *)
          let ce1 = canonize e1 in                                 (*         /      \      =>            /     \        *)
          let ce2 = canonize e2 in                                 (*     MinusPI     \              MinusPP     \       *)
          let ce3 = canonize e3 in                                 (*    /   \         \            /      \      \      *) 
          BinOp(MinusA, BinOp(MinusPP, ce1, ce3, typ2), ce2, typ2) (*  ptr    i     array1        ptr    array1    i     *)
        end
      | x -> x

  let query ctx (q:Queries.t) : Queries.Result.t =
  match q with
  | Queries.MustBeEqual (e1, e2) ->
      begin
        (* Printf.printf "---------------------->   comparing %s and %s \n" (ExpDomain.short 20 (`Lifted e1)) (ExpDomain.short 20 (`Lifted e2)); *)
        if Expcompare.compareExp (canonize e1) (canonize e2) then
          `Bool (true)
        else
          Queries.Result.top()
      end
  | Queries.MayBeEqual (e1, e2) ->
    begin
      (* TODO *)
      Queries.Result.top ()
    end
  | Queries.MayBeLess (e1, e2) ->
    begin
      (* TODO *)
      Queries.Result.top ()
    end
  | _ -> Queries.Result.top ()


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

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
