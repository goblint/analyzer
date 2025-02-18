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
  include Analyses.ValueContexts(D)
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
        begin match a.f (Queries.MayPointTo (mkAddrOf (Var v,offs))) with
          | ad when not (Queries.AD.is_top ad) ->
            Queries.AD.iter (function
                | Queries.AD.Addr.Addr mval -> warn_lval st mval
                | _ -> ()
              ) ad
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
      let do_exp a e =
        match ask.f (Queries.ReachableFrom e) with
        | ad when not (Queries.AD.is_top ad) ->
          ad
          |> Queries.AD.filter (function
              | Queries.AD.Addr.Addr _ -> true
              | _ -> false)
          |> Queries.AD.join a
        (* Ignore soundness warnings, as invalidation proper will raise them. *)
        | _ -> AD.empty () (* TODO: correct?! *)
      in
      List.fold_left do_exp (AD.empty ()) args
    in
    let vars =
      reachable
      |> AD.to_var_may
      |> List.concat_map to_addrs
      |> AD.of_list
    in
    if D.is_top st
    then D.top ()
    else D.filter (fun x -> AD.mem x vars) st

  let get_concrete_lval (ask: Queries.ask) (lval:lval) =
    match ask.f (Queries.MayPointTo (mkAddrOf lval)) with
    | ad when Queries.AD.cardinal ad = 1 && not (Queries.AD.mem UnknownPtr ad) ->
      Queries.AD.Addr.to_mval (Queries.AD.choose ad)
    | _ -> None

  let get_concrete_exp (exp:exp) gl (st:D.t) =
    match constFold true exp with
    | CastE (_,Lval (Var v, offs))
    | Lval (Var v, offs) -> Some (Var v,offs)
    | _ -> None

  let might_be_null (ask: Queries.ask) lv gl st =
    match ask.f (Queries.MayPointTo (mkAddrOf lv)) with
    | ad when not (Queries.AD.is_top ad) ->
      let one_addr_might = function
        | Queries.AD.Addr.Addr mval ->
          D.exists (fun addr -> GobOption.exists (fun x -> is_prefix_of mval x) (Addr.to_mval addr)) st
        | _ -> false
      in
      Queries.AD.exists one_addr_might ad
    | _ -> false

  (*
    Transfer functions and alike
  *)

  (* One step tf-s *)
  let assign man (lval:lval) (rval:exp) : D.t =
    warn_deref_exp (Analyses.ask_of_man man) man.local (Lval lval) ;
    warn_deref_exp (Analyses.ask_of_man man) man.local rval;
    match get_concrete_exp rval man.global man.local, get_concrete_lval (Analyses.ask_of_man man) lval with
    | Some rv, Some mval when might_be_null (Analyses.ask_of_man man) rv man.global man.local ->
      D.add (Addr.of_mval mval) man.local
    | _ -> man.local

  let branch man (exp:exp) (tv:bool) : D.t =
    warn_deref_exp (Analyses.ask_of_man man) man.local exp;
    man.local

  let body man (f:fundec) : D.t =
    man.local

  let return_addr_ = ref Addr.NullPtr
  let return_addr () = !return_addr_

  let return man (exp:exp option) (f:fundec) : D.t =
    let remove_var x v = List.fold_left (Fun.flip @@ D.remove) x (to_addrs v) in
    let nst = List.fold_left remove_var man.local (f.slocals @ f.sformals) in
    match exp with
    | Some ret ->
      warn_deref_exp (Analyses.ask_of_man man) man.local ret;
      begin match get_concrete_exp ret man.global man.local with
        | Some ev when might_be_null (Analyses.ask_of_man man) ev man.global man.local ->
          D.add (return_addr ()) nst
        | _ -> nst  end
    | None -> nst

  (* Function calls *)

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let nst = remove_unreachable (Analyses.ask_of_man man) args man.local in
    Option.iter (fun x -> warn_deref_exp (Analyses.ask_of_man man) man.local (Lval x)) lval;
    List.iter (warn_deref_exp (Analyses.ask_of_man man) man.local) args;
    [man.local,nst]

  let combine_env man lval fexp f args fc au f_ask =
    let cal_st = remove_unreachable (Analyses.ask_of_man man) args man.local in
    D.union (D.remove (return_addr ()) au) (D.diff man.local cal_st)

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    match lval, D.mem (return_addr ()) au with
    | Some lv, true ->
      begin match get_concrete_lval (Analyses.ask_of_man man) lv with
        | Some mval -> D.add (Addr.of_mval mval) man.local
        | _ -> man.local
      end
    | _ -> man.local

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    Option.iter (fun x -> warn_deref_exp (Analyses.ask_of_man man) man.local (Lval x)) lval;
    List.iter (warn_deref_exp (Analyses.ask_of_man man) man.local) arglist;
    let desc = LibraryFunctions.find f in
    match desc.special arglist, lval with
    | Malloc _, Some lv ->
      begin
        match get_concrete_lval (Analyses.ask_of_man man) lv with
        | Some mval ->
          man.split man.local [Events.SplitBranch ((Lval lv), true)];
          man.split (D.add (Addr.of_mval mval) man.local) [Events.SplitBranch ((Lval lv), false)];
          raise Analyses.Deadcode
        | _ -> man.local
      end
    | _ -> man.local

  let name () = "malloc_null"

  let startstate v = D.empty ()
  let threadenter man ~multiple lval f args = [D.empty ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.empty ()

  let init marshal =
    return_addr_ :=  Addr.of_var (Cilfacade.create_var @@ makeVarinfo false "RETURN" voidType)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
