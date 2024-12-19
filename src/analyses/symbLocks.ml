(** Symbolic lockset analysis for per-element (field or index) locking patterns ([symb_locks]).

    @see <https://doi.org/10.1145/2970276.2970337> Static race detection for device drivers: the Goblint approach. Section 5 and 6. *)

module LF = LibraryFunctions
module LP = SymbLocksDomain.LockingPattern
module Exp = SymbLocksDomain.Exp
module ILock = SymbLocksDomain.ILock
module VarEq = VarEq.Spec

module PS = SetDomain.ToppedSet (LP) (struct let topname = "All" end)

open Batteries
open GoblintCil
open Pretty
open Analyses

(* Note: This is currently more conservative than varEq --- but
   it should suffice for tests. *)
module Spec =
struct
  include Analyses.DefaultSpec

  module D = SymbLocksDomain.Symbolic
  include Analyses.ValueContexts(D)

  let name () = "symb_locks"

  let startstate v = D.top ()
  let threadenter man ~multiple lval f args = [D.top ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()

  let branch man exp tv = man.local
  let body   man f = man.local

  let invalidate_exp ask exp st =
    D.filter (fun e -> not (VarEq.may_change ask exp e)) st

  let invalidate_lval ask lv st =
    invalidate_exp ask (mkAddrOf lv) st

  let assign man lval rval = invalidate_lval (Analyses.ask_of_man man) lval man.local

  let return man exp fundec =
    List.fold_right D.remove_var (fundec.sformals@fundec.slocals) man.local

  let enter man lval f args = [(man.local,man.local)]
  let combine_env man lval fexp f args fc au f_ask = au
  let combine_assign man lval fexp f args fc st2 f_ask = man.local

  let get_locks e st =
    let add_perel x xs =
      match LP.from_exps e x with
      | Some x -> PS.add x xs
      | None -> xs
    in
    D.fold add_perel st (PS.empty ())

  let get_all_locks (ask: Queries.ask) e st : PS.t =
    let exps =
      match ask.f (Queries.EqualSet e) with
      | a when not (Queries.ES.is_bot a) -> Queries.ES.add e a
      | _ -> Queries.ES.singleton e
    in
    if M.tracing then M.tracel "symb_locks" "get_all_locks exps %a = %a" d_plainexp e Queries.ES.pretty exps;
    if M.tracing then M.tracel "symb_locks" "get_all_locks st = %a" D.pretty st;
    let add_locks x xs = PS.union (get_locks x st) xs in
    let r = Queries.ES.fold add_locks exps (PS.empty ()) in
    if M.tracing then M.tracel "symb_locks" "get_all_locks %a = %a" d_plainexp e PS.pretty r;
    r

  let same_unknown_index (ask: Queries.ask) exp slocks =
    let uk_index_equal = Queries.must_be_equal ask in
    let lock_index ei ee x xs =
      match Exp.one_unknown_array_index x with
      | Some (true, i, e) when uk_index_equal ei i ->
        PS.add (zero, ee, e) xs
      | _ -> xs
    in
    match Exp.one_unknown_array_index exp with
    | Some (_, i, e) -> D.fold (lock_index i e) slocks (PS.empty ())
    | _ -> PS.empty ()

  let special man lval f arglist =
    let desc = LF.find f in
    match desc.special arglist, f.vname with
    | Lock { lock; _ }, _ ->
      D.add (Analyses.ask_of_man man) lock man.local
    | Unlock lock, _ ->
      D.remove (Analyses.ask_of_man man) lock man.local
    | _, _ ->
      let st =
        match lval with
        | Some lv -> invalidate_lval (Analyses.ask_of_man man) lv man.local
        | None -> man.local
      in
      let write_args =
        LibraryDesc.Accesses.find_kind desc.accs Write arglist
      in
      (* TODO: why doesn't invalidate_exp involve any reachable for deep write? *)
      List.fold_left (fun st e -> invalidate_exp (Analyses.ask_of_man man) e st) st write_args


  module A =
  struct
    module PLock =
    struct
      include CilType.Offset
      let name () = "p-lock"

      let pretty = d_offset (text "*")
      include Printable.SimplePretty (
        struct
          type nonrec t = t
          let pretty = pretty
        end
        )
    end
    module E = Printable.Either (PLock) (ILock)
    include SetDomain.Make (E)

    let name () = "symblock"
    let may_race lp lp2 = disjoint lp lp2
    let should_print lp = not (is_empty lp)
  end

  let add_per_element_access man e rw =
    (* Per-element returns a triple of exps, first are the "element" pointers,
       in the second and third positions are the respectively access and mutex.
       Access and mutex expressions have exactly the given "elements" as "prefixes".

       To know if a access-mutex pair matches our per-element pattern we listify
       the offset (adding dereferencing to our special offset type). Then we take
       the longest common prefix till a dereference and check if the rest is "concrete".
    *)
    let one_perelem (e,a,l) xs =
      (* ignore (printf "one_perelem (%a,%a,%a)\n" Exp.pretty e Exp.pretty a Exp.pretty l); *)
      if M.tracing then M.tracel "symb_locks" "one_perelem (%a,%a,%a)" Exp.pretty e Exp.pretty a Exp.pretty l;
      match Exp.fold_offs (Exp.replace_base (dummyFunDec.svar,`NoOffset) e l) with
      | Some (v, o) ->
        (* ignore (printf "adding lock %s\n" l); *)
        A.add (`Left o) xs
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
        let lock = ILock.of_mval (v, o) in
        A.add (`Right lock) xs
      | _ ->
        Messages.info ~category:Unsound "Internal error: found a strange lockstep pattern.";
        xs
    in
    let do_perel e xs =
      match get_all_locks (Analyses.ask_of_man man) e man.local with
      | a
        when not (PS.is_top a || PS.is_empty a)
        -> PS.fold one_perelem a xs
      | _ -> xs
    in
    let do_lockstep e xs =
      match same_unknown_index (Analyses.ask_of_man man) e man.local with
      | a
        when not (PS.is_top a || PS.is_empty a)
        -> PS.fold one_lockstep a xs
      | _ -> xs
    in
    let matching_exps =
      Queries.ES.meet
        (match man.ask (Queries.EqualSet e) with
         | es when not (Queries.ES.is_top es || Queries.ES.is_empty es)
           -> Queries.ES.add e es
         | _ -> Queries.ES.singleton e)
        (match man.ask (Queries.Regions e) with
         | ls when not (Queries.LS.is_top ls || Queries.LS.is_empty ls)
           -> let add_exp x xs =
                try Queries.ES.add (Mval.Exp.to_cil_exp x) xs
                with Lattice.BotValue -> xs
           in begin
             try Queries.LS.fold add_exp ls (Queries.ES.singleton e)
             with Lattice.TopValue -> Queries.ES.top () end
         | _ -> Queries.ES.singleton e)
    in
    Queries.ES.fold do_lockstep matching_exps
      (Queries.ES.fold do_perel matching_exps (A.empty ()))

  let access man (a: Queries.access) =
    match a with
    | Point ->
      A.empty ()
    | Memory {exp = e; _} ->
      add_per_element_access man e false
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
