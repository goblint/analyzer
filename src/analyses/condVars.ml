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
  let rec var_in_lval p (lh,offs) = var_in_offs p offs && match lh with
  | Var v -> p v
  | Mem e -> var_in_expr p e
  and var_in_offs p = function
  | NoOffset  -> true
  | Field (_,o) -> var_in_offs p o
  | Index (e,o) -> var_in_expr p e && var_in_offs p o
  and var_in_expr p = function
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> true
  | Lval l | AddrOf l | StartOf l -> var_in_lval p l
  | SizeOfE e | AlignOfE e | UnOp (_,e,_) | CastE (_,e) -> var_in_expr p e
  | BinOp (_,e1,e2,_) -> var_in_expr p e1 && var_in_expr p e2
  | Question (c,t,e,_) -> var_in_expr p c && var_in_expr p t && var_in_expr p e
  | AddrOfLabel _ -> true (* TODO *)
  let filter_exprs_with_var p = filter (fun _ v -> V.for_all (var_in_expr p) v)
  (* when local variables go out of scope -> remove them (keys and exprs containing them) *)
  let remove_vars p d =
    filter (fun (v,_) _ -> p v) d (* apply predicate for filtering *)
    |> filter_exprs_with_var p
  let remove_fun_locals f d =
    let p v = not @@ List.mem v (f.sformals @ f.slocals) in
    remove_vars p d
  let only_globals d =
    let p v = v.vglob in
    remove_vars p d
  let only_global_exprs s = V.for_all (var_in_expr (fun v -> v.vglob)) s
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

  (* possible solutions for functions:
    * 1. only intra-procedural
    * 2. enter: remove current locals, return: remove current locals
    * 3. enter: only keep globals, combine: update caller's state with globals from call
    * 4. same, but also consider escaped vars
    *)

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* D.only_globals ctx.local *)
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.only_globals ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    (* combine caller's state with globals from callee *)
    (* TODO (precision): globals with only global vars are kept, the rest is lost -> collect which globals are assignet to *)
    D.merge (fun k s1 s2 -> match s2 with Some ss2 when (fst k).vglob && D.only_global_exprs ss2 -> s2 | _ when (fst k).vglob -> None | _ -> s1) ctx.local au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
