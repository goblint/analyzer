open GobConfig
open Messages
open Progress
open Pretty

module GU = Goblintutil

module Make 
  (Var: Analyses.VarType)  
  (VDom: Lattice.S) 
  (G: Glob.S) =
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  module VarSet = Set.Make(Var)
  
  let solve (system: system) (initialvars: variable list) (start:(variable * var_domain) list): solution' =
    let recal = VMap.create 113 true in
    let sigma = VMap.create 113 (VDom.bot ()) in
    let theta = GMap.create 113 (GDom.bot ()) in
    let vInfl = VMap.create 113 ([]: constrain list) in
    let gInfl = GMap.create 113 ([]: constrain list) in
    let todo  = VMap.create 113 ([]: rhs list) in
    let next_wls = ref VarSet.empty in
    let worklist = ref initialvars in
    let globals_changed = ref true in

    let rec constrainOneVar (x: variable) =
      let rhsides = 
        if VMap.mem recal x then
          let temp = VMap.find todo x in 
          VMap.remove todo x; temp
        else begin
          if not (VMap.mem sigma x) then
            VMap.add sigma x (VDom.bot ()); 
          VMap.add recal x false;
          system x
        end
      in 
      
      let addOneRHS local_state (f: rhs) =
        let doOneGlobalDelta = function 
            | `L (v, state) ->
              if not ( VDom.leq state (VDom.bot ()) ) then
                (* If a variable has become live we must solve it "manually" 
                   because there are no dependecies to it yet. *)
                begin if not (VMap.mem sigma v) then constrainOneVar v end;
                let oldstate = VMap.find sigma v in
                let compls = VDom.join oldstate state in
                  if not (VDom.leq compls oldstate) then begin
                    let add_to_next (x,f) = next_wls := VarSet.add x !next_wls in
                    List.iter add_to_next (VMap.find vInfl v);
                    VMap.remove vInfl v;
                    globals_changed := true;  
                    VMap.replace sigma v compls
                  end
                  
          | `G (g, gstate) -> 
            if not ( GDom.leq gstate (GDom.bot ()) ) then
              let oldgstate = GMap.find theta g in
              let compgs = GDom.join oldgstate gstate in
                if not (GDom.leq compgs oldgstate) then begin
                  let add_to_next (x,f) = next_wls := VarSet.add x !next_wls in
                  List.iter add_to_next (GMap.find gInfl g);
                  GMap.remove gInfl g;
                  globals_changed := true;  
                  GMap.replace theta g compgs
                end
        in
		  let (nls,tc) = f (vEval (x,f), gEval (x,f)) doOneGlobalDelta in
          if get_bool "exp.eclipse" then show_add_work_buf (List.length tc);
          List.iter constrainOneVar tc;
          VDom.join local_state nls
      in

      if rhsides = [] then ()
      else begin
        let old_state   = VMap.find sigma x in
        let local_state = List.fold_left addOneRHS old_state rhsides in
          if tracing then tracei "sol" "Entered %a.\n" Var.pretty_trace x;
          if tracing then trace "sol" "Current state:\n    %a\n" VDom.pretty old_state;
          if not (VDom.leq local_state old_state) then begin
            if tracing then traceu "sol" "Set state to:\n    %a\n" VDom.pretty local_state;
            VMap.replace sigma x local_state;
            let influenced_vars = ref [] in
            let collectInfluence (y,f) = 
              VMap.replace todo y (f :: VMap.find todo y);
              influenced_vars := y :: !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
              if get_bool "exp.eclipse" then show_add_work_buf (List.length !influenced_vars);
              List.iter constrainOneVar !influenced_vars;
          end else 
            if tracing then traceu "sol" "State didn't change!\n"
      end;
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
      VMap.add todo v (system v);
      worklist := v :: !worklist
    in
    List.iter add_start start ;
    while !globals_changed do
      globals_changed := false;
      
      if get_bool "exp.eclipse" then show_add_work_buf (List.length !worklist);
      List.iter constrainOneVar !worklist;
        
      let add_to_work v =
        worklist := v :: !worklist;
        VMap.replace todo v (system v)
      in
      VarSet.iter add_to_work !next_wls;
      next_wls := VarSet.empty;
    done ;
    
    GU.may_narrow := false;
    VMap.clear recal;
    if get_bool "exp.eclipse" then show_subtask "Result Postprocess" 0;  
    List.iter constrainOneVar initialvars;

      (sigma, theta)
end 
