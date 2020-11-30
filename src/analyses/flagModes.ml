(** Flag state values. *)

open Prelude.Ana
open Analyses
open GobConfig

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "fmode"
  module D = FlagModeDomain.Dom
  module C = FlagModeDomain.Dom
  module G = Lattice.Unit

  let flag_list = ref []

  let init () = flag_list := List.map Json.string @@ get_list "ana.osek.flags"

  let eval_int ask exp =
    match ask (Queries.EvalInt exp) with
    | `Int l -> Some l
    | _      -> None

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    if ctx.local = D.bot() then ctx.local else
      match lval with (*TODO keep `Bot*)
      | (Var f,NoOffset) when List.mem f.vname !flag_list -> begin
          if D.mem f ctx.local then begin
            if ((D.find f ctx.local) = `Bot) then begin
              ctx.local
            end else begin
              match eval_int ctx.ask rval with
              | Some ex -> D.add f (`Lifted (false,true,ex)) ctx.local
              | _ -> D.remove f ctx.local
            end
          end else begin
            match eval_int ctx.ask rval with
            | Some ex -> D.add f (`Lifted (false,true,ex)) ctx.local
            | _ -> D.remove f ctx.local
          end
        end
      | _ -> ctx.local
  (*        Some ex when List.mem f.vname !flag_list -> D.add f (`Lifted (false,true,ex)) ctx.local
            | (Var f,NoOffset), _ -> D.remove f ctx.local
            | _ -> ctx.local
            match lval, eval_int ctx.ask rval with
            	| (Var f,NoOffset), Some ex when List.mem f.vname !flag_list -> D.add f (`Lifted (false,true,ex)) ctx.local
            	| (Var f,NoOffset), _ -> D.remove f ctx.local
            	| _ -> ctx.local*)

  let branch ctx (exp:exp) (tv:bool) : D.t =
    if ctx.local = D.bot() then ctx.local else begin
      match%distr (tv, (constFold false exp)) with
      | false, BinOp(Ne,ex,Lval (Var f, NoOffset),_) (*not neq*)
      | false, BinOp(Ne,Lval (Var f, NoOffset), ex,_) (*not neq*)
      | true, BinOp(Eq,ex,Lval (Var f, NoOffset),_)
      | true, BinOp(Eq,Lval (Var f, NoOffset), ex,_) when List.mem f.vname !flag_list -> begin
          let temp = eval_int ctx.ask ex in
          match temp with
          | Some value -> begin (*guard == value = (true true value*)
              try
                match (D.find f ctx.local) with
                | `Lifted (false,_,old_val) -> if value <> old_val then D.add f `Bot ctx.local else ctx.local
                | `Lifted (true,true,old_val) -> if value <> old_val then D.add f `Bot ctx.local else ctx.local
                | `Lifted (true,false,old_val) -> if value <> old_val then D.add f (`Lifted (true, true, value)) ctx.local else D.add f `Bot ctx.local
                | `Top -> D.add f (`Lifted (true, true, value)) ctx.local
                | `Bot -> ctx.local
              with Not_found (*top*) -> D.add f (`Lifted (true, true, value)) ctx.local
            end
          | None -> ctx.local
        end
      | false, Lval (Var f, NoOffset) when List.mem f.vname !flag_list  -> begin (* f == 0*)
          let value = 0L in (*guard == 0 = (true true 0*)
          try
            match (D.find f ctx.local) with
            | `Lifted (false,_,old_val) -> if value <> old_val then D.add f `Bot ctx.local else ctx.local
            | `Lifted (true,true,old_val) -> if value <> old_val then D.add f `Bot ctx.local else ctx.local
            | `Lifted (true,false,old_val) -> if value <> old_val then D.add f (`Lifted (true, true, value)) ctx.local else D.add f `Bot ctx.local
            | `Top -> D.add f (`Lifted (true, true, value)) ctx.local
            | `Bot -> ctx.local
          with Not_found (*top*) -> D.add f (`Lifted (true, true, value)) ctx.local
        end
      | true, Lval (Var f, NoOffset) when List.mem f.vname !flag_list  -> begin (* f != 0*)
          let value = 0L in (*guard != 0 = (true false 0*)
          try
            match (D.find f ctx.local) with
            | `Lifted (false,_,old_val) -> if value <> old_val then ctx.local else D.add f `Bot ctx.local
            | `Lifted (true,true,old_val) -> if value <> old_val then ctx.local else D.add f `Bot ctx.local
            | `Lifted (true,false,old_val) -> ctx.local
            | `Top -> D.add f (`Lifted (true, false, value)) ctx.local
            | `Bot -> ctx.local
          with Not_found (*top*) -> D.add f (`Lifted (true, false, value)) ctx.local
        end
      | false, BinOp(Eq,ex,Lval (Var f, NoOffset),_) (*not eq*)
      | false, BinOp(Eq,Lval (Var f, NoOffset), ex,_) (*not eq*)
      | true, BinOp(Ne,ex,Lval (Var f, NoOffset),_)
      | true, BinOp(Ne,Lval (Var f, NoOffset), ex,_) when List.mem f.vname !flag_list -> begin
          let temp = eval_int ctx.ask ex in
          match temp with
          | Some value -> begin
              try
                match (D.find f ctx.local) with
                | `Lifted (false,_,old_val) -> if value <> old_val then ctx.local else D.add f `Bot ctx.local
                | `Lifted (true,true,old_val) -> if value <> old_val then ctx.local else D.add f `Bot ctx.local
                | `Lifted (true,false,old_val) -> ctx.local
                | `Top -> D.add f (`Lifted (true, false, value)) ctx.local
                | `Bot -> ctx.local
              with Not_found (*top*) -> D.add f (`Lifted (true, false, value)) ctx.local
            end
          | None -> ctx.local
        end
      | _ -> ctx.local
    end

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.top ()
  let threadenter ctx lval f args = D.top ()
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
