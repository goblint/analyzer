(** Must equality between variables and logical expressions. *)

open Batteries
open Cil
open Pretty
open Analyses

module M = Messages
let sprint f x = Pretty.sprint 80 (f () x)

module Domain = struct
  module V =  Queries.ES
  include MapDomain.MapBot (Lval.CilLval) (V)
  (* let rec lval_in_lval = (=) (* TODO *) *)
  (* and lval_in_expr lval = function *)
  (* | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> false *)
  (* | Lval x | AddrOf x | StartOf x -> lval_in_lval lval x *)
  (* | SizeOfE x | AlignOfE x | UnOp (_,x,_) | CastE (_,x) -> lval_in_expr lval x *)
  (* | BinOp (_,a,b,_) -> lval_in_expr lval a || lval_in_expr lval b *)
  (* | Question (c,t,e,_) -> lval_in_expr lval c || lval_in_expr lval t || lval_in_expr lval e *)
  (* | AddrOfLabel _ -> false (* TODO *) *)
  (* let remove_exprs_with_lval lval = filter (fun _ v -> not @@ V.exists (lval_in_expr lval) v) *)
  let get k d = if mem k d then let v = find k d in if V.cardinal v = 1 then Some v else None else None
  let has k d = get k d |> Option.is_some
end

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "condvars"
  module D = Domain
  module C = Domain
  module G = Lattice.Unit

  let (%?) = Option.bind

  let mayPointTo ctx exp =
    match ctx.ask (Queries.MayPointTo exp) with
    | `LvalSet a when not (Queries.LS.is_top a) && Queries.LS.cardinal a > 0 ->
        let top_elt = (dummyFunDec.svar, `NoOffset) in
        let a' = if Queries.LS.mem top_elt a then (
            M.debug_each @@ "mayPointTo: query result for " ^ sprint d_exp exp ^ " contains TOP!"; (* UNSOUND *)
            Queries.LS.remove top_elt a
          ) else a
        in
        Queries.LS.elements a'
    | _ -> []

  let mustPointTo ctx exp = (* this is just to get CilLval *)
    match mayPointTo ctx exp with
    | [clval] -> Some clval
    | _ -> None

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    let d = ctx.local in
    let of_q = function Queries.CondVars e -> Some e | _ -> None in
    let of_expr = function
      | Lval lval -> Some lval (* TODO accept more exprs *)
      | _ -> None
    in
    let of_lval lval = mustPointTo ctx (AddrOf lval) in
    let of_clval k = if D.mem k d then Some (`ExprSet (D.find k d)) else None in
    of_q q %? of_expr %? of_lval %? of_clval |? Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* remove all vars that lval may point to *)
    let d = List.fold_left (flip D.remove) ctx.local (mayPointTo ctx (AddrOf lval)) in
    let save_expr lval expr =
      match mustPointTo ctx (AddrOf lval) with
      | Some clval -> D.add clval (D.V.singleton expr) d (* if lval must point to clval, add expr *)
      | None -> d
    in
    let is_cmp = function Lt | Gt | Le | Ge | Eq | Ne -> true | _ -> false in
    match rval with
    | BinOp (op, _, _, _) when is_cmp op -> (* logical expression *)
        save_expr lval rval
    | Lval k when Option.is_some @@ mustPointTo ctx (AddrOf k) %? flip D.get d -> (* var-eq for transitive closure *)
        save_expr lval rval
    | _ -> d

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

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

let _ =
  MCP.register_analysis (module Spec : Spec)
