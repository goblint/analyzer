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

  let rec vars_from_lval (l: lval) : varinfo list  = 
    let vars_written_to =  
      match l with
      | Var v, _ ->  (
          if (Cil.isFunctionType v.vtype) then [] else [v]  (*I do not want functions in the set of live variables*)
        )
      | Mem m, _ -> vars_from_expr m
    in

    let vars_in_offset = 
      match l with
      | Var _, off -> vars_from_offset off
      | Mem _, off ->  Logs.debug "(!) vars_in_offset used"; vars_from_offset off
    in

    (vars_written_to @ vars_in_offset)

  and vars_from_offset (off: offset) : varinfo list =
    match off with
    | NoOffset -> []
    | Field (_, off) -> vars_from_offset off (* what to do with fieldinfo?*)
    | Index (e, off) -> 
      let vars_in_e = vars_from_expr e in
      let vars_in_off = vars_from_offset off in
      (match vars_in_off with
       | [] -> []
       | vars_in_off ->  (vars_in_e @ vars_in_off))

  and vars_from_expr (e: exp) : varinfo list =
    let rec aux acc e =
      match e with
      | Lval v -> vars_from_lval v @ acc
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
      | CastE (_, e1)         ->   aux acc e1
      | AddrOf (l1)          ->   (match vars_from_lval l1 with
          | [] -> acc
          |  v -> (v @ acc)
        )
      (* | AddrOfLabel _ -> Logs.debug "(!) Expression of type AddrOfLabel"; acc
         | StartOf l1 -> Logs.debug "(!) Expression of type StartOf"; acc
         | Const _ ->Logs.debug "(!) Expression of type Const";  acc
         | Real _ -> Logs.debug "(!) Expression of type Real"; acc
         | Imag _ -> Logs.debug "(!) Expression of type Imag"; acc
         | SizeOf _ -> Logs.debug "(!) Expression of type SizeOf"; acc
         | AlignOf _ -> Logs.debug "(!) Expression of type AlignOf"; acc 
         | SizeOfStr _ -> Logs.debug "(!) Expression of type SizeOfStr"; acc *)
      | _ ->  acc 

    in

    (* let give_exp_type e = 
       match e with
        | Const _ -> Logs.debug "(!) Expression of type Const"
        | Lval _ -> Logs.debug "(!) Expression of type Lval"
        | SizeOf _ -> Logs.debug "(!) Expression of type SizeOf"
        | Real _ -> Logs.debug "(!) Expression of type Real"
        | Imag _ -> Logs.debug "(!) Expression of type Imag"
        | SizeOfE _ -> Logs.debug "(!) Expression of type SizeOfE"
        | SizeOfStr _ -> Logs.debug "(!) Expression of type SizeOfSTr"
        | AlignOf _ -> Logs.debug "(!) Expression of type AlignOf"
        | AlignOfE _ -> Logs.debug "(!) Expression of type AlignOfE"
        | UnOp _ -> Logs.debug "(!) Expression of type UnOp"
        | BinOp _ -> Logs.debug "(!) Expression of type BinOp"
        | Question _ -> Logs.debug "(!) Expression of type Question"
        | CastE _ -> Logs.debug "(!) Expression of type CastE"
        | AddrOf _ -> Logs.debug "(!) Expression of type AddrOf"
        | AddrOfLabel _ -> Logs.debug "(!) Expression of type AddrOfLabel"
        | StartOf _ -> Logs.debug "(!) Expression of type StartOf"
        | _ -> Logs.debug "(!) Impossible: Expression of unknown type"
       in
       give_exp_type e; *)

    aux [] e



  let assign man man_forw (lval:lval) (rval:exp) =
    let v = vars_from_lval lval in

    (* This is wrong. If the variabes describe a memory location, they should instead all be added to the set of live variables!*)
    match v with
    | [] -> D.join man.local (D.of_list (vars_from_expr rval)) (*if I do not know what the value is assigned to, then all RHS-Variables might be relevant*)
    | v-> 
      let l = (D.diff man.local (D.of_list v)) in
      if (List.exists (fun elem -> D.mem elem man.local) v) then D.join l (D.of_list (vars_from_expr rval)) (*if anything on the rhs is important, this is live now*)
      else (
        let loc = M.Location.Node man.node in
        (match v with
         | v::_ -> M.warn ~loc:loc  "Unnecessary assignment to variable %s, as it is not live at this program point" v.vname
         | [] -> () (*this case is already handled above*)
        ); l)

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
    if branch_irrelevant then (D.of_list (vars_from_expr exp))
    else D.join man.local  (D.of_list (vars_from_expr exp))

  let body man man_forw (f:fundec) =
    man.local

  (* TODO *)
  let enter man man_forw (lval: lval option) (f:fundec) (args:exp list) =
    (* Logs.debug "=== enter function %s with args %s ===" f.svar.vname 
       (String.concat ", " (List.map (CilType.Exp.show) args)); *)

    (* let vars =
       match lval with 
       | None -> man.local
       | Some lv -> man.local (*i have to check for every arg ... no wait... I do not care about the args here, i care about those at the combine!!!!*)

       in *)

    [man.local, man.local]

  (* TODO *)
  let combine_env man man_forw (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    (* Logs.debug "=== combine_env of function %s ===" f.svar.vname;
       let args_pretty = String.concat ", " (List.map CilType.Exp.show args) in
       Logs.debug "    args: %s" args_pretty;

       let sformals_pretty = String.concat ", " (List.map (fun v -> v.vname) f.sformals) in
       Logs.debug "    sformals: %s" sformals_pretty; *)

    (*map relevant sformals in man.local to the corresponding variables contained in the argument*)

    let arg_formal_pairs = List.combine args f.sformals in
    let relevant_arg_vars = 
      List.fold_left (fun acc (arg_exp, formal_var) ->
          if D.mem formal_var au then
            D.join acc (D.of_list(vars_from_expr arg_exp))
          else
            acc
        ) (D.empty()) arg_formal_pairs
    in

    (*join relevant*)
    D.join man.local relevant_arg_vars 

  let combine_assign man man_forw (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    (* SHOULD JUST USE THE SIMPLE ASSIGN I ALREADY IMPLEMENT *)
    let exp_vars = D.of_list(vars_from_expr fexp) in 
    (* 
    Logs.debug "(!) combine_assign: fexp = %s" (CilType.Exp.show fexp);
    (* Type of the expression:*)
    let exp_type = Cil.typeOf fexp in
    Logs.debug "(!) combine_assign: type of fexp = %s" (CilType.Typ.show exp_type);
    Logs.debug "(!) combine_assign: exp_vars = %s" (String.concat ", " (List.map (fun v -> v.vname) (D.elements exp_vars))); *)

    (* this is problematic. I should only remove the lvar-vars if lval is a simple variable. If it is used to reference memory the variabes are actually wuite important*)
    match lval with 
    | Some lval -> 
      let lval_vars = D.of_list (vars_from_lval lval) in
      if (D.exists (fun e -> D.mem e man.local) lval_vars) then (
        let a = (D.union man.local exp_vars) in
        D.diff a lval_vars)
      else man.local
    |  _ -> man.local


  (** A transfer function which handles the return statement, i.e.,
      "return exp" or "return" in the passed function (fundec) *)
  let return man man_forw (exp: exp option) (f:fundec) : D.t =

    (* this does not really work that well, as I pass all live vars which does not generally make the function important *)
    let return_val_is_important = (not (D.is_bot man.local)) || (String.equal f.svar.vname "main") in (*this does not take globals int account, only checks for "temp"*)

    match exp with
    | None -> D.empty()
    | Some e -> if return_val_is_important
      then D.of_list (vars_from_expr e)
      else D.empty()


  let special man man_forw (lval: lval option) (f:varinfo) (arglist:exp list) =
    man.local

  let threadenter man man_forw ~multiple lval f args = [man.local]
  let threadspawn man man_forw ~multiple lval f args fman = man.local
end
