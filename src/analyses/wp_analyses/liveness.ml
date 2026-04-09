open GoblintCil
open Analyses

module BackwSpec : BackwAnalyses.BackwSpecSpec = functor (ForwSpec : Analyses.Spec) ->
struct

  include BackwAnalyses.DefaultBackwSpec (ForwSpec)
  module C = ForwSpec.C

  (* Adding these module definitions because the "include" of the DefaultBackwSpec is not enough*)
  module D_forw = ForwSpec.D
  module G_forw = ForwSpec.G
  module V_forw = ForwSpec.V
  module P_forw = ForwSpec.P
  let name () = "liveness"

  module G = Lattice.Unit
  module V = EmptyV
  module P = EmptyP

  module LiveVariableSet =  SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All variables" end)
  module D =  LiveVariableSet (*Set of program variables as domain*)

  let startstate v = D.empty()
  let exitstate v = D.empty()

  let rec vars_from_lval (l: lval) man_forw : varinfo list  = 
    let vars_written_to =  
      match l with
      | Var v, _ ->  (
          if (Cil.isFunctionType v.vtype) then [] else [v]  (*I do not want functions in the set of live variables*)
        )
      | Mem exp, _ -> (*If a pointer may point to a variable, these variables are live as well...*)
        let may_point_to = Queries.AD.to_var_may (man_forw.ask (MayPointTo exp)) in
        if may_point_to = [] then (
          M.warn ~category:MessageCategory.Unsound "The expression %a may point to an unknown variable. This makes the analysis unsound." d_exp exp; (*UNSOUND: I do not think that this check is enough. Maybe I should just exclude analyzing programs with variables whose address is taken.*)
          vars_from_expr exp man_forw )
        else (
          Logs.debug "(!) The expression %a may point to the variables %s" d_exp exp (String.concat ", " (List.map (fun v -> v.vname) may_point_to));
          may_point_to @ vars_from_expr exp man_forw)
    in

    let vars_in_offset = 
      match l with
      | Var _, off -> vars_from_offset off man_forw
      | Mem _, off -> vars_from_offset off man_forw
    in

    (vars_written_to @ vars_in_offset)

  and vars_from_offset (off: offset) man_forw : varinfo list =
    match off with
    | NoOffset -> []
    | Field (_, off) -> vars_from_offset off man_forw (* what to do with fieldinfo?*)
    | Index (e, off) -> 
      let vars_in_e = vars_from_expr e man_forw  in
      let vars_in_off = vars_from_offset off man_forw in
      (match vars_in_off with
       | [] -> []
       | vars_in_off ->  (vars_in_e @ vars_in_off))

  and vars_from_expr (e: exp) man_forw : varinfo list =
    let rec aux acc e =
      match e with
      | Lval v -> vars_from_lval v man_forw @ acc 
      | BinOp (_, e1, e2, _)  ->
        let acc1 = aux acc e1 in
        aux acc1 e2
      | UnOp (_, e1, _)       ->  aux acc e1
      | SizeOfE e1            -> aux acc e1
      | AlignOfE e1           -> aux acc e1
      | Question (e1, e2, e3, _) -> 
        let acc1 = aux acc e1 in
        let acc2 = aux acc1 e2 in
        aux acc2 e3
      | CastE (_, e1)         ->   aux acc e1  (*This appears to make problems when building for jobs*)
      | AddrOf (l1)          ->   (match vars_from_lval l1 man_forw with
          | [] -> acc
          |  v -> (v @ acc)
        )
      | _ ->  acc 

    in
    aux [] e

  let rec assign man man_forw (lval:lval) (rval:exp) =
    match lval with
    | Var v, _ -> 
      if (D.mem v man.local || v.vglob) then ( (* Global variables are considered live when writing to them. *)
        let rval_vars = D.of_list (vars_from_expr rval man_forw)
        in
        D.union rval_vars (D.diff man.local (D.singleton v))
      ) else (
        (* let loc = M.Location.Node man.node in *)
        (* M.warn ~loc:loc ~category:MessageCategory.Program "Unnecessary assignment to variable '%s', as it is not live at this program point." v.vname; *)
        man.local
      )
    | Mem exp, _ -> 
      let may_point_to = Queries.AD.to_var_may (man_forw.ask (MayPointTo exp)) in
      let lval_vars = D.of_list (vars_from_expr exp man_forw) in
      let rval_vars = D.of_list (vars_from_expr rval man_forw) in

      match may_point_to with  (*POSSIBLY UNSOUND: could also be an overapproximation, depending on whether assumption is true*)
      | [v] ->
        D.union (assign  man man_forw (Var v, NoOffset) rval) lval_vars  (* We assume that if it my only point to one variable, we can treat this as if we just assigned to that variable*)
      | _ ->  D.union rval_vars (D.union lval_vars man.local)

  let branch man man_forw (exp:exp) (tv:bool) =  
    (* This just randomly asks whether all loops terimante to use getg_forw utilized in man.global *)
    (* let () =
       match man_forw.ask(Queries.MustTermAllLoops) with
       | true -> Logs.debug "MustTermAllLoops is TRUE"
       | _ -> Logs.debug "MustTermAllLoops is NOT TRUE"
       in *)

    let branch_irrelevant : bool = (
      match Queries.eval_bool (Analyses.ask_of_man man_forw) exp with
      | `Lifted b -> tv <> b
      | `Bot -> false
      | `Top -> false
    )
    in
    if branch_irrelevant then (D.of_list (vars_from_expr exp man_forw))
    else D.join man.local (D.of_list (vars_from_expr exp man_forw))

  let body man man_forw (f:fundec) =
    man.local

  let enter man man_forw (lval: lval option) (f:fundec) (args:exp list) =

    match lval with
    | Some (Var v, _) -> 
      if (D.mem v man.local) then ( 
        [man.local, (D.singleton v)]
      ) else (
        [man.local, D.empty()]
      )
    | Some (Mem exp, _) -> [man.local, D.of_list (vars_from_expr exp man_forw)] 
    | None -> [man.local, D.empty()]

  let combine_env man man_forw (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =

    (* map relevant sformals in man.local to the corresponding variables contained in the argument*)
    let arg_formal_pairs = List.combine args f.sformals in
    let relevant_arg_vars = 
      List.fold_left (fun acc (arg_exp, formal_var) ->
          if D.mem formal_var au then
            D.join acc (D.of_list(vars_from_expr arg_exp man_forw))
          else
            acc
        ) (D.empty()) arg_formal_pairs
    in

    (*join relevant*)
    D.join man.local relevant_arg_vars 

  let combine_assign man man_forw (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =  
    match lval with 
    | None -> man.local 
    | Some l -> assign man man_forw l fexp

  (** A transfer function which handles the return statement, i.e.,
      "return exp" or "return" in the passed function (fundec) *)
  let return man man_forw (exp: exp option) (f:fundec) : D.t =

    let return_val_is_important = (not (D.is_bot man.local)) || (String.equal f.svar.vname "main") in

    match exp with
    | None -> D.empty()
    | Some e -> if return_val_is_important
      then D.of_list (vars_from_expr e man_forw)
      else D.empty()


  let special man man_forw (lval: lval option) (f:varinfo) (arglist:exp list) =
    (* log when called  *)
    Logs.debug "(!) Called special for function %s with arguments %s" f.vname (String.concat ", " (List.map (fun e -> Pretty.sprint ~width:80 (d_exp () e)) arglist));

    let desc = LibraryFunctions.find f in 
    match desc.special arglist with
    (* Could have some special handeling of library functions here *)
    | _ ->
      let argvars = List.fold_left (fun acc arg -> D.union acc (D.of_list (vars_from_expr arg man_forw))) (D.empty()) arglist in 
      match lval with
      | None -> D.union man.local argvars
      | Some (Var v, _) -> D.union (D.diff man.local (D.singleton(v))) argvars
      | Some (Mem exp, _) -> D.union (D.union argvars (D.of_list (vars_from_expr exp man_forw))) man.local

  let threadenter man man_forw ~multiple lval f args = [man.local]
  let threadspawn man man_forw ~multiple lval f args fman = man.local

  let query man (type a) man_forw (q: a Queries.t): a Queries.result =

    (* Die recursion ist nicht sauber durchdacht *)
    let rec is_dead_assign man man_forw (lval:lval) (rval:exp) (is_dead:bool) : (D.t * bool) =

      match lval with
      | Var v, _ ->
        Logs.debug "D.mem v man.local is %b" (D.mem v man.local);
        Logs.debug "v.glob is %b" v.vglob;
        if (D.mem v man.local || v.vglob) then 
          let rval_vars = D.of_list (vars_from_expr rval man_forw)
          in
          (D.union rval_vars (D.diff man.local (D.singleton v)), false)
        else (
          Logs.debug "Variable '%s' is not live at this program point." v.vname;
          (man.local, true)
        )
      | Mem exp, _ -> (
          Logs.debug "lval is expression";
          let may_point_to = Queries.AD.to_var_may (man_forw.ask (MayPointTo exp)) in
          let lval_vars = D.of_list (vars_from_expr exp man_forw) in
          let rval_vars = D.of_list (vars_from_expr rval man_forw) in

          match may_point_to with  (*POSSIBLY UNSOUND: could also be an overapproximation, depending on whether assumption is true*)
          | [v] ->
            let rec_assign_result, is_dead = 
              match (is_dead_assign  man man_forw (Var v, NoOffset) rval is_dead) with 
              | (res, new_is_dead) -> res, new_is_dead
            in 
            (D.union rec_assign_result lval_vars, is_dead)(* We assume that if it my only point to one variable, we can treat this as if we just assigned to that variable*)
          | _ ->  ((D.union rval_vars (D.union lval_vars man.local)), is_dead)
        )
    in 

    let open Queries in

    match q with
    | IsDeadVar v -> not (D.mem v man.local)
    | MayBeDeadAssignment lval -> (
        Logs.debug "Checking if assignment to lval %a may be dead at node %a with local state %a" d_lval lval Node.pretty_trace man.node D.pretty man.local;
        match is_dead_assign man man_forw lval (Const (CInt (Z.zero, IInt, None))) false with
        | (_, is_dead) -> Logs.debug "isdead is %b" is_dead ; is_dead )
    | _ -> Result.top q
end
