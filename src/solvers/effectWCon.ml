open Messages
open Progress
open Pretty

module GU = Goblintutil

module Make 
  (Var: Analyses.VarType)  (* the equation variables *)
  (VDom: Lattice.S) (* the domain *)
  (G: Global.S) = 
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  module GCache = Cache.OneVar (G.Var)
  
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    let sigma: VDom.t VMap.t = VMap.create 113 (VDom.bot ()) in
    let theta = GMap.create 113 (GDom.bot ()) in
    let vInfl = VMap.create 113 ([]: constrain list) in
    let gInfl = GMap.create 113 ([]: constrain list) in
    let todo  = VMap.create 113 ([]: rhs list) in
    let unsafe = ref ([]: constrain list) in
    let worklist = ref initialvars in

    let rec constrainOneVar (x: variable) =
      let rhsides = 
        let notnew = VMap.mem sigma in 
          if notnew x then
            let temp = VMap.find todo x in VMap.remove todo x; temp
          else begin
            VMap.add sigma x (VDom.bot ());  (* danger! Adding default value!!! If the datastruct refuses this,  membership test will fail -> inf. loop *)
            system x
          end
      in 
	
      begin if rhsides = [] then ()
      else begin
        let local_state = ref (VDom.bot ()) in 
        let constrainOneRHS (f: rhs) =
          let (nls,ngd,tc) = f (vEval (x,f), GCache.cached (gEval (x,f))) in
          let doOneGlobalDelta = function
            | `L (v, state) ->
              if not ( VDom.leq state (VDom.bot ()) ) then
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
                    GMap.replace theta g compgs;
                    incr Goblintutil.globals_changed;
                    unsafe := lst @ !unsafe;
                    GMap.remove gInfl g
                  end
          in
            List.iter doOneGlobalDelta ngd;
            if !GU.eclipse then show_add_work_buf (List.length tc);
            List.iter constrainOneVar tc;
            local_state := VDom.join !local_state nls
        in
          List.iter constrainOneRHS rhsides;
          let old_state = VMap.find sigma x in
          if tracing then tracei "sol" (dprintf "Entered %a.\n" Var.pretty_trace x);
          if tracing then trace "sol" (dprintf "Current state:\n    %a\n" VDom.pretty old_state );
          if not (VDom.leq !local_state old_state) then begin
            if tracing then traceu "sol" (dprintf "Set state to:\n    %a\n" VDom.pretty !local_state );
            VMap.replace sigma x (VDom.join !local_state old_state);
            let influenced_vars = ref [] in
            let collectInfluence (y,f) = 
              VMap.replace todo y (f :: VMap.find todo y);
              influenced_vars := y :: !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
          if !GU.eclipse then show_add_work_buf (List.length !influenced_vars);
              List.iter constrainOneVar !influenced_vars;
          end else 
            if tracing then traceu "sol" (dprintf "State didn't change!\n")
    end end;
    if !GU.eclipse then show_worked_buf 1
          

    and vEval (c: constrain) var =
      if !GU.eclipse then show_add_work_buf 1;
      constrainOneVar var;
      VMap.replace vInfl var (c :: VMap.find vInfl var);
      VMap.find sigma var 
    
    and gEval (c: constrain) glob = 
      GMap.replace gInfl glob (c :: GMap.find gInfl glob);
      GMap.find theta glob 

    in
      GU.may_narrow := false;
      if !GU.eclipse then show_subtask "Constant Propagation" 0;  
      List.iter (fun (v,d) -> VMap.add sigma v d) start ;
      while !worklist != [] do
        if !GU.eclipse then show_add_work_buf (List.length !worklist);
        List.iter constrainOneVar !worklist;
        worklist := [];
        let recallConstraint (y,f) = 
          VMap.replace todo y (f :: VMap.find todo y);
          worklist := y :: !worklist
        in
          List.iter recallConstraint !unsafe;
          unsafe := [];
      done;
      (sigma, theta)
end 
