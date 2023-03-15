(** Must equality between variables and logical expressions. *)
(* TODO: unused, what is this analysis? *)

open Prelude.Ana
open Analyses

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
    | SizeOfE e | AlignOfE e | UnOp (_,e,_) | CastE (_,e) | Imag e | Real e -> var_in_expr p e
    | BinOp (_,e1,e2,_) -> var_in_expr p e1 && var_in_expr p e2
    | Question (c,t,e,_) -> var_in_expr p c && var_in_expr p t && var_in_expr p e
    | AddrOfLabel _ -> true
  let filter_exprs_with_var p = filter (fun _ v -> V.for_all (var_in_expr p) v)
  (* when local variables go out of scope -> remove them (keys and exprs containing them) *)
  let filter_vars p d =
    filter (fun (v,_) _ -> p v) d (* apply predicate for filtering *)
    |> filter_exprs_with_var p
  let remove_var v = filter_vars ((<>) v)
  let remove_fun_locals f d =
    let p v = not @@ List.mem v (f.sformals @ f.slocals) in
    filter_vars p d
  let only_globals d =
    let p v = v.vglob in
    filter_vars p d
  let only_locals d =
    let p v = not v.vglob in
    filter_vars p d
  let only_untainted d tainted =
    let p v = (not v.vglob) || not (TaintPartialContexts.VS.mem v tainted) in
    filter_vars p d
  let only_global_exprs s = V.for_all (var_in_expr (fun v -> v.vglob)) s
  let rec get k d =
    if mem k d && V.cardinal (find k d) = 1 then
      let s = find k d in
      match V.choose s with
      | Lval (Var v, offs) -> get (v, Lval.CilLval.of_ciloffs offs) d (* transitive lookup *)
      | _ -> Some s
    else None
  let get_elt k d = Option.map V.choose @@ get k d
  let has k d = get k d |> Option.is_some
end

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "condvars"
  module D = Domain
  module C = Domain

  (* >? is >>=, |? is >> *)
  let (>?) = Option.bind

  let mayPointTo ctx exp =
    match ctx.ask (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && Queries.LS.cardinal a > 0 ->
      let top_elt = (dummyFunDec.svar, `NoOffset) in
      let a' = if Queries.LS.mem top_elt a then (
          M.info ~category:Unsound "mayPointTo: query result for %a contains TOP!" d_exp exp; (* UNSOUND *)
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
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.CondVars e ->
      let d = ctx.local in
      let rec of_expr tv = function
        | UnOp (LNot, e, t) when isIntegralType t -> of_expr (not tv) e
        | BinOp (Ne, e1, e2, t) when isIntegralType t -> of_expr (not tv) (BinOp (Eq, e1, e2, t))
        | BinOp (Eq, e1, e2, t) when isIntegralType t && e2 = zero -> of_expr (not tv) e1
        | BinOp (Eq, e2, e1, t) when isIntegralType t && e2 = zero -> of_expr (not tv) e1
        | Lval lval -> Some (tv, lval)
        | _ -> None
      in
      let of_lval (tv,lval) = Option.map (fun k -> tv, k) @@ mustPointTo ctx (AddrOf lval) in
      let t tv e = if tv then e else UnOp (LNot, e, intType) in
      (* TODO: remove option? *)
      let f tv v = D.V.map (t tv) v |> fun v -> Some v in
      let of_clval (tv,k) = D.get k d >? f tv in
      of_expr true e >? of_lval >? of_clval |? Queries.Result.top q
    | _ -> Queries.Result.top q

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* remove all keys lval may point to, and all exprs that contain the variables (TODO precision) *)
    let d = List.fold_left (fun d (v,o as k) -> D.remove k d |> D.remove_var v) ctx.local (mayPointTo ctx (AddrOf lval)) in
    let save_expr lval expr =
      match mustPointTo ctx (AddrOf lval) with
      | Some clval ->
        if M.tracing then M.tracel "condvars" "CondVars: saving %a = %a\n" Lval.CilLval.pretty clval d_exp expr;
        D.add clval (D.V.singleton expr) d (* if lval must point to clval, add expr *)
      | None -> d
    in
    let is_cmp = function Lt | Gt | Le | Ge | Eq | Ne -> true | _ -> false in
    match rval with
    | BinOp (op, _, _, _) when is_cmp op -> (* logical expression *)
      save_expr lval rval
    | Lval k when Option.is_some (mustPointTo ctx (AddrOf k) >? flip D.get d) -> (* var-eq for transitive closure *)
      mustPointTo ctx (AddrOf k) >? flip D.get_elt d |> Option.map (save_expr lval) |? d
    | _ -> d

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  (* possible solutions for functions:
    * 1. only intra-procedural <- we do this
    * 2. enter: remove current locals, return: remove current locals
    * 3. enter: only keep globals, combine: update caller's state with globals from call
    * 4. same, but also consider escaped vars
  *)

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* D.only_globals ctx.local *)
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()]

  let combine ctx ~longjmpthrough (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    (* combine caller's state with globals from callee *)
    (* TODO (precision): globals with only global vars are kept, the rest is lost -> collect which globals are assigned to *)
    (* D.merge (fun k s1 s2 -> match s2 with Some ss2 when (fst k).vglob && D.only_global_exprs ss2 -> s2 | _ when (fst k).vglob -> None | _ -> s1) ctx.local au *)
    let tainted = TaintPartialContexts.conv_varset (f_ask.f Queries.MayBeTainted) in
    D.only_untainted ctx.local tainted (* tainted globals might have changed... *)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* TODO: shouldn't there be some kind of invalidadte, depending on the effect of the special function? *)
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
