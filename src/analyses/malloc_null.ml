(** Path-sensitive analysis of failed dynamic memory allocations ([malloc_null]). *)

module AD = ValueDomain.AD
module IdxDom = ValueDomain.IndexDomain
module Offs = ValueDomain.Offs

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Addr = ValueDomain.Addr
  module D = ValueDomain.AddrSetDomain
  module C = ValueDomain.AddrSetDomain
  module P = IdentityP (D)

  (*
    Addr set functions:
  *)
  let is_prefix_of m1 m2 = Option.is_some (Addr.Mval.prefix m1 m2)

  (* We just had to dereference an lval --- warn if it was null *)
  let warn_lval (st:D.t) (v :Addr.Mval.t) : unit =
    try
      if D.exists (fun x -> GobOption.exists (fun x -> is_prefix_of x v) (Addr.to_mval x)) st
      then
        let var = Addr.of_mval v in
        Messages.warn ~category:Messages.Category.Behavior.Undefined.nullpointer_dereference "Possible dereferencing of null on variable '%a'." Addr.pretty var
    with SetDomain.Unsupported _ -> ()

  (* Warn null-lval dereferences, but not normal (null-) lvals*)
  let rec warn_deref_exp (a: Queries.ask) (st:D.t) (e:exp): unit =
    let warn_lval_mem e offs =
      (*      begin try List.iter (warn_lval st) (AD.to_mval (BS.eval_lv gl s (Mem e, offs)))
              with SetDomain.Unsupported _ -> () end;*)
      match e with
      | Lval (Var v, offs) ->
        begin match a.f (Queries.MayPointToA (mkAddrOf (Var v,offs))) with
          | a when not (Queries.AD.is_top a) ->
            Queries.AD.iter (function
                | Queries.AD.Addr.Addr addr -> warn_lval st addr
                | _ -> ()
              ) a
          | _ -> ()
        end
      | _ -> ()
    in
    match e with
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _
    | AddrOfLabel _
    | Lval (Var _, _) -> ()
    | AddrOf (Var _, _)
    | StartOf (Var _, _) ->  warn_lval_mem e NoOffset
    | AddrOf (Mem e, offs)
    | StartOf (Mem e, offs)
    | Lval (Mem e, offs) ->
      warn_deref_exp a st e;
      warn_lval_mem e offs
    | BinOp (_,e1,e2,_) ->
      warn_deref_exp a st e1;
      warn_deref_exp a st e2
    | UnOp (_,e,_)
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE  (_,e) ->
      warn_deref_exp a st e
    | Question (b, t, f, _) ->
      warn_deref_exp a st b;
      warn_deref_exp a st t;
      warn_deref_exp a st f

  (* Generate addresses to all points in an given varinfo. (Depends on type) *)
  let to_addrs (v:varinfo) : Addr.t list =
    let make_offs = List.fold_left (fun o f -> `Field (f, o)) `NoOffset in
    let rec add_fields (base: fieldinfo list) fs acc =
      match fs with
      | [] -> acc
      | f :: fs ->
        match unrollType f.ftype with
        | TComp ({cfields=ffs; _},_) -> add_fields base fs (List.rev_append (add_fields (f::base) ffs []) acc)
        | _                       -> add_fields base fs ((Addr.of_mval (v,make_offs (f::base))) :: acc)
    in
    match unrollType v.vtype with
    | TComp ({cfields=fs; _},_) -> add_fields [] fs []
    | _ -> [Addr.of_var v]

  (* Remove null values from state that are unreachable from exp.*)
  let remove_unreachable (ask: Queries.ask) (args: exp list) (st: D.t) : D.t =
    let reachable =
      let do_exp e =
        match ask.f (Queries.ReachableFromA e) with
        | a when not (Queries.AD.is_top a) ->
          let to_extra addr xs =
            match addr with
            | Queries.AD.Addr.Addr addr -> AD.of_mval addr :: xs
            | _ -> xs
          in
          Queries.AD.fold to_extra a []
        (* Ignore soundness warnings, as invalidation proper will raise them. *)
        | _ -> []
      in
      List.concat_map do_exp args
    in
    let add_exploded_struct (one: AD.t) (many: AD.t) : AD.t =
      let vars = AD.to_var_may one in
      List.fold_right AD.add (List.concat_map to_addrs vars) many
    in
    let vars = List.fold_right add_exploded_struct reachable (AD.empty ()) in
    if D.is_top st
    then D.top ()
    else D.filter (fun x -> AD.mem x vars) st

  let get_concrete_lval (ask: Queries.ask) (lval:lval) =
    match ask.f (Queries.MayPointToA (mkAddrOf lval)) with
    | a when Queries.AD.cardinal a = 1 && not (Queries.AD.mem UnknownPtr a) ->
      Queries.AD.Addr.to_mval (Queries.AD.choose a)
    | _ -> None

  let get_concrete_exp (exp:exp) gl (st:D.t) =
    match constFold true exp with
    | CastE (_,Lval (Var v, offs))
    | Lval (Var v, offs) -> Some (Var v,offs)
    | _ -> None

  let might_be_null (ask: Queries.ask) lv gl st =
    match ask.f (Queries.MayPointToA (mkAddrOf lv)) with
    | a when not (Queries.AD.is_top a) ->
      let one_addr_might = function
        | Queries.AD.Addr.Addr addr ->
          D.exists (fun x -> GobOption.exists (fun x -> is_prefix_of addr x) (Addr.to_mval x)) st
        | _ -> false
      in
      Queries.AD.exists one_addr_might a
    | _ -> false

  (*
    Transfer functions and alike
  *)

  (* One step tf-s *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local (Lval lval) ;
    warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local rval;
    match get_concrete_exp rval ctx.global ctx.local, get_concrete_lval (Analyses.ask_of_ctx ctx) lval with
    | Some rv, Some addr when might_be_null (Analyses.ask_of_ctx ctx) rv ctx.global ctx.local ->
      D.add (Addr.of_mval addr) ctx.local
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local exp;
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return_addr_ = ref Addr.NullPtr
  let return_addr () = !return_addr_

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let remove_var x v = List.fold_right D.remove (to_addrs v) x in
    let nst = List.fold_left remove_var ctx.local (f.slocals @ f.sformals) in
    match exp with
    | Some ret ->
      warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local ret;
      begin match get_concrete_exp ret ctx.global ctx.local with
        | Some ev when might_be_null (Analyses.ask_of_ctx ctx) ev ctx.global ctx.local ->
          D.add (return_addr ()) nst
        | _ -> nst  end
    | None -> nst

  (* Function calls *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let nst = remove_unreachable (Analyses.ask_of_ctx ctx) args ctx.local in
    Option.iter (fun x -> warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local (Lval x)) lval;
    List.iter (warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local) args;
    [ctx.local,nst]

  let combine_env ctx lval fexp f args fc au f_ask =
    let cal_st = remove_unreachable (Analyses.ask_of_ctx ctx) args ctx.local in
    D.union (D.remove (return_addr ()) au) (D.diff ctx.local cal_st)

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    match lval, D.mem (return_addr ()) au with
    | Some lv, true ->
      begin match get_concrete_lval (Analyses.ask_of_ctx ctx) lv with
        | Some addr -> D.add (Addr.of_mval addr) ctx.local
        | _ -> ctx.local
      end
    | _ -> ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    Option.iter (fun x -> warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local (Lval x)) lval;
    List.iter (warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local) arglist;
    let desc = LibraryFunctions.find f in
    match desc.special arglist, lval with
    | Malloc _, Some lv ->
      begin
        match get_concrete_lval (Analyses.ask_of_ctx ctx) lv with
        | Some addr ->
          ctx.split ctx.local [Events.SplitBranch ((Lval lv), true)];
          ctx.split (D.add (Addr.of_mval addr) ctx.local) [Events.SplitBranch ((Lval lv), false)];
          raise Analyses.Deadcode
        | _ -> ctx.local
      end
    | _ -> ctx.local

  let name () = "malloc_null"

  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.empty ()

  let init marshal =
    return_addr_ :=  Addr.of_var (Cilfacade.create_var @@ makeVarinfo false "RETURN" voidType)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
