(** Helper analysis to be path-sensitive in failed dynamic memory allocations ([malloc_null]). It is not soundness critical, it only causes certain paths to be kept separate. *)

module AD = ValueDomain.AD
module IdxDom = ValueDomain.IndexDomain
module Offs = ValueDomain.Offs

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  module Addr = ValueDomain.Addr
  module D = ValueDomain.AddrSetDomain
  include Analyses.ValueContexts(D)
  module P = IdentityP (D)

  (*
    Addr set functions:
  *)
  let is_prefix_of m1 m2 = Option.is_some (Addr.Mval.prefix m1 m2)

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
        | _ -> a
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
    | CastE (_,_,Lval (Var v, offs))
    | Lval (Var v, offs) -> Some (Var v,offs)
    | _ -> None

  let might_be_null (ask: Queries.ask) lv gl st =
    match ask.f (Queries.MayPointTo (mkAddrOf lv)) with
    | ad when not (Queries.AD.is_top ad) ->
      let one_addr_might = function
        | Queries.AD.Addr.Addr mval ->
          D.exists (fun addr -> GobOption.exists (is_prefix_of mval) (Addr.to_mval addr)) st
        | _ -> false
      in
      Queries.AD.exists one_addr_might ad
    | _ -> false

  (*
    Transfer functions and alike
  *)

  (* One step tf-s *)
  let assign man (lval:lval) (rval:exp) : D.t =
    match get_concrete_exp rval man.global man.local, get_concrete_lval (Analyses.ask_of_man man) lval with
    | Some rv, Some mval when might_be_null (Analyses.ask_of_man man) rv man.global man.local ->
      D.add (Addr.of_mval mval) man.local
    | _ -> man.local

  let return_addr_ = ref Addr.NullPtr
  let return_addr () = !return_addr_

  let return man (exp:exp option) (f:fundec) : D.t =
    let remove_var x v = List.fold_left (Fun.flip D.remove) x (to_addrs v) in
    let nst = List.fold_left remove_var man.local (f.slocals @ f.sformals) in
    BatOption.map_default (fun ret ->
        match get_concrete_exp ret man.global man.local with
        | Some ev when might_be_null (Analyses.ask_of_man man) ev man.global man.local ->
          D.add (return_addr ()) nst
        | _ -> nst
      ) nst exp

  (* Function calls *)

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let nst = remove_unreachable (Analyses.ask_of_man man) args man.local in
    let args = GobList.combine_short f.sformals args in
    let res = List.fold_left (fun acc (v, e) ->
        BatOption.map_default (fun rv ->
            if might_be_null (Analyses.ask_of_man man) rv man.global man.local then
              D.add (Addr.of_var v) acc
            else
              acc
          ) acc (get_concrete_exp e man.global man.local)
      ) (D.empty ()) args
    in
    [man.local,D.join res nst]

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
  let exitstate  v = D.empty ()

  let init marshal =
    return_addr_ :=  Addr.of_var (Cilfacade.create_var @@ makeVarinfo false "RETURN" voidType)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
