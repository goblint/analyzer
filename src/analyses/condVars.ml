(** Symbolic variable - logical expression equalities analysis ([condvars]). *)
(* TODO: unused, what is this analysis? *)

open Batteries
open GoblintCil
open Analyses

module Domain = struct
  module V =  Queries.ES
  include MapDomain.MapBot (Mval.Exp) (V)
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
    let p v = not @@ List.mem_cmp CilType.Varinfo.compare v (f.sformals @ f.slocals) in
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
      | Lval (Var v, offs) -> get (v, Offset.Exp.of_cil offs) d (* transitive lookup *)
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
  include Analyses.ValueContexts(D)

  (* >? is >>=, |? is >> *)
  let (>?) = Option.bind

  let mayPointTo man exp =
    let ad = man.ask (Queries.MayPointTo exp) in
    let a' = if Queries.AD.mem UnknownPtr ad then (
        M.info ~category:Unsound "mayPointTo: query result for %a contains TOP!" d_exp exp; (* UNSOUND *)
        Queries.AD.remove UnknownPtr ad
      ) else ad
    in
    List.filter_map (function
        | ValueDomain.Addr.Addr (v,o) -> Some (v, ValueDomain.Addr.Offs.to_exp o) (* TODO: use unconverted addrs in domain? *)
        | _ -> None
      ) (Queries.AD.elements a')

  let mustPointTo man exp = (* this is just to get Mval.Exp *)
    match mayPointTo man exp with
    | [clval] -> Some clval
    | _ -> None

  (* queries *)
  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.CondVars e ->
      let d = man.local in
      let rec of_expr tv = function
        | UnOp (LNot, e, t) when isIntegralType t -> of_expr (not tv) e
        | BinOp (Ne, e1, e2, t) when isIntegralType t -> of_expr (not tv) (BinOp (Eq, e1, e2, t))
        | BinOp (Eq, e1, e2, t) when isIntegralType t && e2 = zero -> of_expr (not tv) e1
        | BinOp (Eq, e2, e1, t) when isIntegralType t && e2 = zero -> of_expr (not tv) e1
        | Lval lval -> Some (tv, lval)
        | _ -> None
      in
      let of_lval (tv,lval) = Option.map (fun k -> tv, k) @@ mustPointTo man (AddrOf lval) in
      let t tv e = if tv then e else UnOp (LNot, e, intType) in
      (* TODO: remove option? *)
      let f tv v = D.V.map (t tv) v |> fun v -> Some v in
      let of_clval (tv,k) = D.get k d >? f tv in
      of_expr true e >? of_lval >? of_clval |? Queries.Result.top q
    | _ -> Queries.Result.top q

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    (* remove all keys lval may point to, and all exprs that contain the variables (TODO precision) *)
    let d = List.fold_left (fun d (v,o as k) -> D.remove k d |> D.remove_var v) man.local (mayPointTo man (AddrOf lval)) in
    let save_expr lval expr =
      match mustPointTo man (AddrOf lval) with
      | Some clval ->
        if M.tracing then M.tracel "condvars" "CondVars: saving %a = %a" Mval.Exp.pretty clval d_exp expr;
        D.add clval (D.V.singleton expr) d (* if lval must point to clval, add expr *)
      | None -> d
    in
    let is_cmp = function Lt | Gt | Le | Ge | Eq | Ne -> true | _ -> false in
    match rval with
    | BinOp (op, _, _, _) when is_cmp op -> (* logical expression *)
      save_expr lval rval
    | Lval k -> (* var-eq for transitive closure *)
      mustPointTo man (AddrOf k) >? flip D.get_elt d |> Option.map (save_expr lval) |? d
    | _ -> d

  let branch man (exp:exp) (tv:bool) : D.t =
    man.local

  (* possible solutions for functions:
    * 1. only intra-procedural <- we do this
    * 2. enter: remove current locals, return: remove current locals
    * 3. enter: only keep globals, combine: update caller's state with globals from call
    * 4. same, but also consider escaped vars
  *)

  let body man (f:fundec) : D.t =
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    (* D.only_globals man.local *)
    man.local

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, D.bot ()]

  let combine_env man lval fexp f args fc au (f_ask: Queries.ask) =
    (* combine caller's state with globals from callee *)
    (* TODO (precision): globals with only global vars are kept, the rest is lost -> collect which globals are assigned to *)
    (* D.merge (fun k s1 s2 -> match s2 with Some ss2 when (fst k).vglob && D.only_global_exprs ss2 -> s2 | _ when (fst k).vglob -> None | _ -> s1) man.local au *)
    let tainted = TaintPartialContexts.conv_varset (f_ask.f Queries.MayBeTainted) in
    D.only_untainted man.local tainted (* tainted globals might have changed... *)

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    man.local

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* TODO: shouldn't there be some kind of invalidadte, depending on the effect of the special function? *)
    man.local

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.bot ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
