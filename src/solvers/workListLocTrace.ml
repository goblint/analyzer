open Prelude
open Analyses
open Constraints
open PostSolvingFlag
open PriorityCalc

module Make =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct

    include Generic.SolverStats (S) (HM)

    module ListQueue = struct
      type 'a t = 'a list
      exception Empty
      let empty = []
      let is_empty = function [] -> true | _ -> false
      let to_list = Fun.id
      let from_list = Fun.id
      let enqueue st elem =
        let stList = to_list st
        in 
        if List.mem elem stList then (
          st
        ) 
        else
          List.append st [elem]
      let peek = function [] -> raise Empty | x :: _ -> x
      let pop = function [] -> raise Empty | x :: s -> x, s
      let size = List.length

      let fold f a b = List.fold_left f b (to_list a)

    end

    module VS = ListQueue

    open S.Dom

    module SVarMap = Map.Make(Node)

    let eq x get set =
      match S.system x with
      | None -> bot ()
      | Some f ->
        eval_rhs_event x;
        f get set

    let solve _ st vs =
      print_string ("solve wurde aufgerufen mit 
      st=["^(List.fold (fun s_fold (a, b) -> "("^(Node.show (S.Var.node a))^",\n "^(S.Dom.show b)^");\n "^s_fold) "" st)^"]
      \nvs="^(List.fold (fun s_fold a -> (Node.show (S.Var.node a))^"; "^s_fold) "" vs)^"\n");
      let infl = HM.create 10 in
      let rho  = HM.create 10 in
      let vs   = ref (VS.from_list vs) in
      let init x =
        new_var_event x;
        HM.replace rho x (bot ());
        HM.replace infl x VS.empty;
      in
      let eval x y =
        get_var_event y;
        HM.replace infl y (VS.enqueue (try HM.find infl y with Not_found -> VS.empty) x);
        try HM.find rho y
        with Not_found ->
          new_var_event y;
          HM.replace rho y (bot ());
          vs := VS.enqueue !vs y;
          bot ()
      in
      let set x d =
        let old = try HM.find rho x with Not_found -> init x; bot () in
        if not (leq d old) then begin
          update_var_event x old d;
          HM.replace rho x (join old d);
          let q = try HM.find infl x with Not_found -> VS.empty in
          HM.replace infl x VS.empty;
          let xNode = S.Var.node x
          in
          if predominatorRegistration#isLoopHead xNode  then
            let qList, qMap = List.fold (fun (listAcc, mapAcc) svElem -> 
                let svarNode = S.Var.node svElem
                in
                (svarNode::listAcc, SVarMap.add svarNode svElem mapAcc)) ([], SVarMap.empty) (VS.to_list q)

            in
            let nonPrio, prio = predominatorRegistration#getPriorityNodePartition xNode qList 
            in
            predominatorRegistration#printOut ();
            (* prio und NonPrio mal ausgeben und gucken, ob das das ist, was ich erwarte *)
            print_string ("qList:"^(List.fold (fun acc node -> (Node.show node)^"; "^acc) "" qList)^"
      \nnonPrio:"^(List.fold (fun acc node -> (Node.show node)^"; "^acc) "" nonPrio)^"
      \nprio:"^(List.fold (fun acc node -> (Node.show node)^"; "^acc) "" prio)^"\n");
      vs := List.fold ( fun acc node -> VS.enqueue acc (SVarMap.find node qMap)) !vs prio;
            vs := List.fold ( fun acc node -> VS.enqueue acc (SVarMap.find node qMap)) !vs nonPrio;
          else 
            vs := (VS.fold VS.enqueue q !vs)
        end
      in
      start_event ();
      (* das scheinen initialen rho-Werte zu sein *)
      let _ = List.iter (fun (x,d) -> HM.add rho x d) st in
      while (not (VS.is_empty !vs))&&(not (omitPostSolving#getFlag () )) do
        print_string "Another iteration in workListLocTrace\n";
        let x, vs' = VS.pop !vs in
        let _ = vs := vs' in
        set x (eq x (eval x) set);
        if omitPostSolving#getFlag ()  then print_string "We have an error trace, so solver should stop right here\n" 
      done;
      if (omitPostSolving#getFlag () ) then Messages.warn_noloc "Program contains an error trace";
      stop_event ();
      print_string "Solver is done and returns rho\n";
      rho
  end


let _ =
  Selector.add_solver ("WLLocTrac",  (module EqIncrSolverFromEqSolver (Make)));
