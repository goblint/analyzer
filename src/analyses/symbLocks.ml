(** Symbolic lock-sets for use in per-element patterns. *)

module LF = LibraryFunctions
module LP = Exp.LockingPattern
module Exp = Exp.Exp
module VarEq = VarEq.Spec

open Prelude.Ana
open Analyses

(* Note: This is currently more conservative than varEq --- but
   it should suffice for tests. *)
module Spec =
struct
  include Analyses.DefaultSpec

  exception Top

  module D = LockDomain.Symbolic
  module C = LockDomain.Symbolic
  module G = Lattice.Unit

  let name () = "symb_locks"

  let startstate v = D.top ()
  let threadenter ctx f args = D.top ()
  let threadcombine ctx f args fd = D.bot ()
  let exitstate  v = D.top ()

  let branch ctx exp tv = ctx.local
  let body   ctx f = ctx.local

  let invalidate_exp ask exp st =
    D.filter (fun e -> not (VarEq.may_change ask exp e)) st

  let invalidate_lval ask lv st =
    invalidate_exp ask (mkAddrOf lv) st

  let assign ctx lval rval = invalidate_lval ctx.ask lval ctx.local

  let return ctx exp fundec =
    List.fold_right D.remove_var (fundec.sformals@fundec.slocals) ctx.local

  let enter ctx lval f args = [(ctx.local,ctx.local)]
  let combine ctx lval fexp f args fc st2 = ctx.local

  let get_locks e st =
    let add_perel x xs =
      match LP.from_exps e x with
      | Some x -> Queries.PS.add x xs
      | None -> xs
    in
    D.fold add_perel st (Queries.PS.empty ())

  let get_all_locks ask e st : Queries.PS.t =
    let exps =
      match ask (Queries.EqualSet e) with
      | `ExprSet a when not (Queries.ES.is_bot a) -> Queries.ES.add e a
      | _ -> Queries.ES.singleton e
    in
    let add_locks x xs = Queries.PS.union (get_locks x st) xs in
    Queries.ES.fold add_locks exps (Queries.PS.empty ())

  let same_unknown_index ask exp slocks =
    let uk_index_equal i1 i2 =
      match ask (Queries.ExpEq (i1, i2)) with
      | `Bot | `Bool true -> true
      | _ -> false
    in
    let lock_index ei ee x xs =
      match Exp.one_unknown_array_index x with
      | Some (true, i, e) when uk_index_equal ei i ->
        Queries.PS.add (zero, ee, e) xs
      | _ -> xs
    in
    match Exp.one_unknown_array_index exp with
    | Some (_, i, e) -> D.fold (lock_index i e) slocks (Queries.PS.empty ())
    | _ -> Queries.PS.empty ()

  let special ctx lval f arglist =
    match LF.classify f.vname arglist with
    | `Lock _ ->
      D.add ctx.ask (List.hd arglist) ctx.local
    | `Unlock ->
      D.remove ctx.ask (List.hd arglist) ctx.local
    | `Unknown fn when VarEq.safe_fn fn ->
      Messages.warn ("Assume that "^fn^" does not change lockset.");
      ctx.local
    | `Unknown x -> begin
        let st =
          match lval with
          | Some lv -> invalidate_lval ctx.ask lv ctx.local
          | None -> ctx.local
        in
        let write_args =
          match LF.get_invalidate_action f.vname with
          | Some fnc -> fnc `Write arglist
          | _ -> arglist
        in
        List.fold_left (fun st e -> invalidate_exp ctx.ask e st) st write_args
      end
    | _ ->
      ctx.local

  module ExpSet = BatSet.Make (Exp)
  let type_inv_tbl = Hashtbl.create 13
  let type_inv (c:compinfo) : Lval.CilLval.t list =
    try [Hashtbl.find type_inv_tbl c,`NoOffset]
    with Not_found ->
      let i = Goblintutil.create_var (makeGlobalVar ("(struct "^c.cname^")") (TComp (c,[]))) in
      Hashtbl.add type_inv_tbl c i;
      [i, `NoOffset]

  let rec conv_const_offset x =
    match x with
    | NoOffset    -> `NoOffset
    | Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_const_offset o)
    | Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_const_offset o)
    | Field (f,o) -> `Field (f, conv_const_offset o)

  let one_perelem ask (e,a,l) es =
    (* Type invariant variables. *)
    let b_comp = Exp.base_compinfo e a in
    let f es (v,o) =
      match Exp.fold_offs (Exp.replace_base (v,o) e l) with
      | Some (v,o) -> ExpSet.add (Lval (Var v,o)) es
      | None -> es
    in
    match b_comp with
    | Some ci -> List.fold_left f es (type_inv ci)
    | None -> es

  let one_lockstep (_,a,m) ust =
    let rec conv_const_offset x =
      match x with
      | NoOffset    -> `NoOffset
      | Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_const_offset o)
      | Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_const_offset o)
      | Field (f,o) -> `Field (f, conv_const_offset o)
    in
    match m with
    | AddrOf (Var v,o) ->
      LockDomain.Lockset.add (ValueDomain.Addr.from_var_offset (v, conv_const_offset o),true) ust
    | _ ->
      ust

  let may_race (ctx1,ac1) (ctx,ac2) =
    match ac1, ac2 with
    | `Lval (l1,r1), `Lval (l2,r2) ->
      let ls1 = get_all_locks ctx1.ask (Lval l1) ctx1.local in
      let ls1 = Queries.PS.fold (one_perelem ctx1.ask) ls1 (ExpSet.empty) in
      let ls2 = get_all_locks ctx.ask (Lval l2) ctx.local in
      let ls2 = Queries.PS.fold (one_perelem ctx.ask) ls2 (ExpSet.empty) in
      (*ignore (Pretty.printf "{%a} inter {%a} = {%a}\n" (Pretty.d_list ", " Exp.pretty) (ExpSet.elements ls1) (Pretty.d_list ", " Exp.pretty) (ExpSet.elements ls2) (Pretty.d_list ", " Exp.pretty) (ExpSet.elements (ExpSet.inter ls1 ls2)));*)
      ExpSet.is_empty (ExpSet.inter ls1 ls2) &&
      let ls1 = same_unknown_index ctx1.ask (Lval l1) ctx1.local in
      let ls1 = Queries.PS.fold one_lockstep ls1 (LockDomain.Lockset.empty ()) in
      let ls2 = same_unknown_index ctx.ask (Lval l2) ctx.local in
      let ls2 = Queries.PS.fold one_lockstep ls2 (LockDomain.Lockset.empty ()) in
      LockDomain.Lockset.is_empty (LockDomain.Lockset.ReverseAddrSet.inter ls1 ls2)

    | _ -> true

  let query ctx (q:Queries.t) =
    match q with
    | _ -> Queries.Result.top ()

  let add_per_element_access ctx e rw =
    let module LSSet = Access.LSSet in
    (* Per-element returns a triple of exps, first are the "element" pointers,
       in the second and third positions are the respectively access and mutex.
       Access and mutex expressions have exactly the given "elements" as "prefixes".

       To know if a access-mutex pair matches our per-element pattern we listify
       the offset (adding dereferencing to our special offset type). Then we take
       the longest common prefix till a dereference and check if the rest is "concrete".
    *)
    let one_perelem (e,a,l) xs =
      (* ignore (printf "one_perelem (%a,%a,%a)\n" Exp.pretty e Exp.pretty a Exp.pretty l); *)
      match Exp.fold_offs (Exp.replace_base (dummyFunDec.svar,`NoOffset) e l) with
      | Some (v, o) ->
        let l = Pretty.sprint 80 (d_offset (text "*") () o) in
        (* ignore (printf "adding lock %s\n" l); *)
        LSSet.add ("p-lock",l) xs
      | None -> xs
    in
    (* Array lockstep also returns a triple of exps. Second and third elements in
       triples are access and mutex exps. Common index is replaced with *.
       First element is unused.

       To find if this pattern matches, we try to separate the base variable and
       the index from both -- access exp and mutex exp. We check if indexes match
       and the rest is concrete. Then replace the common index with *. *)
    let one_lockstep (_,a,m) xs =
      match m with
      | AddrOf (Var v,o) ->
        let lock = ValueDomain.Addr.from_var_offset (v, conv_const_offset o) in
        LSSet.add ("i-lock",ValueDomain.Addr.short 80 lock) xs
      | _ ->
        Messages.warn "Internal error: found a strange lockstep pattern.";
        xs
    in
    let do_perel e xs =
      match get_all_locks ctx.ask e ctx.local with
      | a
        when not (Queries.PS.is_top a || Queries.PS.is_empty a)
        -> Queries.PS.fold one_perelem a xs
      | _ -> xs
    in
    let do_lockstep e xs =
      match same_unknown_index ctx.ask e ctx.local with
      | a
        when not (Queries.PS.is_top a || Queries.PS.is_empty a)
        -> Queries.PS.fold one_lockstep a xs
      | _ -> xs
    in
    let matching_exps =
      Queries.ES.meet
        (match ctx.ask (Queries.EqualSet e) with
         | `ExprSet es when not (Queries.ES.is_top es || Queries.ES.is_empty es)
           -> Queries.ES.add e es
         | _ -> Queries.ES.singleton e)
        (match ctx.ask (Queries.Regions e) with
         | `LvalSet ls when not (Queries.LS.is_top ls || Queries.LS.is_empty ls)
           -> let add_exp x xs =
                try Queries.ES.add (Lval.CilLval.to_exp x) xs
                with Lattice.BotValue -> xs
           in begin
             try Queries.LS.fold add_exp ls (Queries.ES.singleton e)
             with Lattice.TopValue -> Queries.ES.top () end
         | _ -> Queries.ES.singleton e)
    in
    Queries.ES.fold do_lockstep matching_exps
      (Queries.ES.fold do_perel matching_exps (LSSet.empty ()))

  let part_access ctx e v _ =
    let open Access in
    let ls = add_per_element_access ctx e false in
    (* ignore (printf "bla %a %a = %a\n" d_exp e D.pretty ctx.local LSSet.pretty ls); *)
    (LSSSet.singleton (LSSet.empty ()), ls)
end

let _ =
  MCP.register_analysis (module Spec : Spec)
