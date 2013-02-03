(** Dont use this! *)
open GobConfig
open Messages
open Progress
open Pretty

module GU = Goblintutil

module Make 
  (Var: Analyses.VarType)  (* the equation variables *)
  (VDom: Lattice.S) (* the domain *)
  (G: Glob.S) =
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    let recal = VMap.create 113 true in
    let sigma: VDom.t VMap.t = VMap.create 113 (VDom.bot ()) in
    let theta = GMap.create 113 (GDom.bot ()) in
    let vInfl = VMap.create 113 ([]: constrain list) in
    let gInfl = GMap.create 113 ([]: constrain list) in
    let todo  = VMap.create 113 ([]: rhs list) in
    let unsafe = ref ([]: constrain list) in
    let worklist = ref initialvars in

    let rec constrainOneVar (x: variable) =
      let rhsides = 
        if not (VMap.mem recal x) then begin
          if not (VMap.mem sigma x) then
            VMap.add sigma x (VDom.bot ());  
          VMap.add recal x false;
          system x
        end else begin
          let temp = VMap.find todo x in 
          VMap.remove todo x; temp
        end
      in 

      begin if rhsides = [] then ()
      else begin
        let constrainOneRHS old_state (f: rhs) =
          let doOneGlobalDelta = function 
            | `L (v, state) ->
              if not ( VDom.leq state (VDom.bot ()) ) then
                (* If a variable has become live we must solve it "manually" 
                   because there are no dependecies to it yet. *)
                begin if not (VMap.mem sigma v) then constrainOneVar v end;
                let oldstate = VMap.find sigma v in
                let compls = VDom.join oldstate state in
                  if not (VDom.leq compls oldstate) then begin
                    let lst = VMap.find vInfl v in
                    VMap.replace sigma v compls;
                    unsafe := lst @ !unsafe;
                    VMap.remove vInfl v
                  end
            | `G (g, gstate) ->        
              if not ( GDom.leq gstate (GDom.bot ()) ) then
                let oldgstate = GMap.find theta g in
                let compgs = GDom.join oldgstate gstate in
                  if not (GDom.leq compgs oldgstate) then begin
                    let lst = GMap.find gInfl g in
                    GMap.replace theta g (GDom.join oldgstate compgs);
                    unsafe := lst @ !unsafe;
                    GMap.remove gInfl g
                  end
          in
			let (nls,tc) = f (vEval (x,f), gEval (x,f)) doOneGlobalDelta in
            if get_bool "exp.eclipse" then show_add_work_buf (List.length tc);
            worklist := tc @ !worklist;
            VDom.join old_state nls
        in
          (* widen *)
          let old_w = VMap.find sigma x in
          let con_w = List.fold_left constrainOneRHS old_w rhsides in
          let new_w = VDom.widen old_w con_w in
          
          if not (VDom.leq new_w old_w) then begin
            VMap.replace sigma x new_w;
            let influenced_vars = ref [] in
            let collectInfluence (y,f) = 
              VMap.replace todo y (f :: VMap.find todo y);
              influenced_vars := y :: !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
              if get_bool "exp.eclipse" then show_add_work_buf (List.length !influenced_vars);
              List.iter constrainOneVar !influenced_vars;
(*               worklist := !influenced_vars @ !worklist; *)
              if tracing then traceu "sol" "Set state to:\n    %a\n" VDom.pretty new_w
          end else 
            if tracing then traceu "sol" "State didn't change!\n" ;


          (* narrow *)
          let old_n = VMap.find sigma x in
          let con_n = List.fold_left constrainOneRHS (VDom.bot ()) rhsides in
          let new_n = VDom.narrow old_n con_n in

          if tracing then tracei "sol" "Narrowing %a.\n" Var.pretty_trace x;
          if tracing then trace "sol" "Old state:\n    %a\n" VDom.pretty old_n;

          if not (VDom.leq old_n new_n) then begin
            VMap.replace sigma x new_n;
            let influenced_vars = ref [] in
            let collectInfluence (y,f) = 
              VMap.replace todo y (f :: VMap.find todo y);
              influenced_vars := y :: !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
              if get_bool "exp.eclipse" then show_add_work_buf (List.length !influenced_vars);
              List.iter constrainOneVar !influenced_vars;
(*               worklist := !influenced_vars @ !worklist; *)
              if tracing then traceu "sol" "Set state to:\n    %a\n" VDom.pretty new_w
          end else
            if tracing then traceu "sol" "State didn't change!\n" 
    end end;
    if get_bool "exp.eclipse" then show_worked_buf 1
          

    and vEval (c: constrain) var =
      if get_bool "exp.eclipse" then show_add_work_buf 1;
      constrainOneVar var;
      VMap.replace vInfl var (c :: VMap.find vInfl var);
      VMap.find sigma var
    
    and gEval (c: constrain) glob = 
      GMap.replace gInfl glob (c :: GMap.find gInfl glob);
      GMap.find theta glob 

    in
      GU.may_narrow := true;
      if get_bool "exp.eclipse" then show_subtask "Constant Propagation" 0;  
      let add_start (v,d) = 
        VMap.add sigma v d;
        let edges = system v in
        VMap.add todo v edges;
        worklist := v :: !worklist
      in
      List.iter add_start start ;
      while !worklist != [] do
        if get_bool "exp.eclipse" then show_add_work_buf (List.length !worklist);
        let wl = !worklist in worklist := [];
        List.iter constrainOneVar wl;
        let recallConstraint (y,f) = 
          VMap.replace todo y (f :: VMap.find todo y);
          worklist := y :: !worklist
        in
          List.iter recallConstraint !unsafe;
          unsafe := [];
      done;
      
      VMap.clear recal;
      GU.may_narrow := false;
      worklist := initialvars;
      if get_bool "exp.eclipse" then show_subtask "Reporting Phase" 0;  
      while !worklist != [] do
        if get_bool "exp.eclipse" then show_add_work_buf (List.length !worklist);
        let wl = !worklist in worklist := [];
        List.iter constrainOneVar wl;
        let recallConstraint (y,f) = 
          VMap.replace todo y (f :: VMap.find todo y);
          worklist := y :: !worklist
        in
          List.iter recallConstraint !unsafe;
          unsafe := [];
      done;
      (sigma, theta)
end 
