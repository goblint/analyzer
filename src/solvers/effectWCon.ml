open GobConfig 
open Messages
open Progress
open Pretty

module GU = Goblintutil

let vars = ref 0
let evals = ref 0

module Make 
  (Var: Analyses.VarType)  (* the equation variables *)
  (VDom: Lattice.S) (* the domain *)
  (G: Glob.S) =
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  module GCache = Cache.OneVar (G.Var)
  
  let rec iter f = function 
    | [] -> ()
    | x::[] -> f x
    | x::(y::xs) -> f x; f y; iter f xs
    
  let stack_d = ref 0
  let full_trace = false
  let start_c = 0  
  let max_c   : int ref = ref (-1) 
  let max_var : Var.t option ref = ref None 
  
  let is_some = function 
    | Some _ -> true
    | _ -> false
  
  let from_some = function
    | Some x -> x
    | None -> raise Not_found
  
  let histo = Hashtbl.create 1024
  let increase (v:Var.t) = 
    let set v c = 
      if not full_trace && (c > start_c && c > !max_c && (not (is_some !max_var) || not (Var.equal (from_some !max_var) v))) then begin
        if tracing then trace "sol" "Switched tracing to %a\n" Var.pretty_trace v;
        max_c := c;
        max_var := Some v
      end
    in
    try let c = Hashtbl.find histo v in
        set v (c+1);
        Hashtbl.replace histo v (c+1)
    with Not_found -> begin
        set v 1;
        Hashtbl.add histo v 1
    end
  
  let cons_unique key x xs =
    let xk = key x in
    if List.exists (fun y -> xk = key y) xs
    then xs
    else x::xs
  
  
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    let sigma: VDom.t VMap.t = VMap.create 113000 (VDom.bot ()) in
    let theta = GMap.create 113000 (GDom.bot ()) in
    let vInfl = VMap.create 113000 ([]: (constrain * int) list) in
    let gInfl = GMap.create 113000 ([]: (constrain * int) list) in
    let todo  = VMap.create 113000 ([]: (rhs * int) list) in
    let unsafe = ref ([]: (constrain * int) list) in
    let workset = ref initialvars in
    
    let rec constrainOneVar (x: variable) =
      let rhsides = 
        let notnew = VMap.mem sigma in 
          if notnew x then
            let temp = VMap.find todo x in VMap.remove todo x; temp
          else begin
            vars := !vars + 1;
            if tracing && Var.category x = 3 then trace "sol" "New %a\n" Var.pretty_trace x;
            VMap.add sigma x (VDom.bot ());  (* danger! Adding default value!!! If the datastruct refuses this,  membership test will fail -> inf. loop *)
            fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system x) ([],0))
          end
      in 
	
      begin if rhsides=[] then ()
      else begin
        let local_state = ref (VDom.bot ()) in 
        let constrainOneRHS (f, i) =
          evals := !evals + 1;
          (if (get_bool "dbg.solver-progress") then (incr stack_d; print_int !stack_d; flush stdout)); 
          let doOneGlobalDelta = function
            | `L (v, state) ->
              if not ( VDom.leq state (VDom.bot ()) ) then
                let oldstate = VMap.find sigma v in
                (* If a variable has become live we must solve it "manually" 
                   because there are no dependecies to it yet. *)
                begin if not (VMap.mem sigma v) then constrainOneVar v end;
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
                    if (get_bool "dbg.verbose") then begin ignore (fprintf stderr "\n********************GLOBALS CHANGED********************* (%d)\n" !Goblintutil.globals_changed); flush stderr end;
                    unsafe := lst @ !unsafe;
                    GMap.remove gInfl g
                  end
          in
          let (nls,tc) = f (vEval ((x,f),i), GCache.cached (gEval ((x,f),i))) doOneGlobalDelta in
            iter constrainOneVar tc;
            local_state := VDom.join !local_state nls;
            if (get_bool "dbg.solver-progress") then decr stack_d 
        in
          iter constrainOneRHS rhsides;
          let old_state = VMap.find sigma x in
          (if tracing then increase x); (*update the histogram*)
          if full_trace || ((not (VDom.is_bot old_state)) && is_some !max_var && Var.equal (from_some !max_var) x) then begin
            if tracing then tracei "sol" "(%d) Entered %a.\n" !max_c Var.pretty_trace x;
            if tracing then traceu "sol" "%a\n\n" VDom.pretty_diff (!local_state, old_state)
          end;
          let new_val = VDom.join !local_state old_state in
          if not (VDom.leq new_val old_state) then begin
            VMap.replace sigma x new_val;
            let influenced_vars = ref [] in
            let collectInfluence ((y,f),i) = 
              VMap.replace todo y (cons_unique snd (f,i) (VMap.find todo y));             
              influenced_vars := y :: !influenced_vars
            in
              iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
              iter constrainOneVar !influenced_vars
          end 
    end end
          

    and vEval (c: constrain * int) var =
      constrainOneVar var;
      VMap.replace vInfl var (c :: VMap.find vInfl var);
      VMap.find sigma var 
    
    and gEval (c: constrain * int) glob = 
      GMap.replace gInfl glob (c :: GMap.find gInfl glob);
      GMap.find theta glob 

    in
      GU.may_narrow := false;
      let add_start (v,d) = 
        VMap.add sigma v d;
        let edges = fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system v) ([],0)) in
        VMap.add todo v edges;
        workset := v :: !workset
      in
      iter add_start start ;
      while not ([] = !workset) do
        iter constrainOneVar !workset;
        workset := [];
        let recallConstraint ((y,f),i) = 
          VMap.replace todo y (cons_unique snd (f,i) (VMap.find todo y));
          workset := y :: !workset;
        in
          iter recallConstraint !unsafe;
          unsafe := []
      done;
      (sigma, theta)
end 



module Make2
  (S:Analyses.GlobConstrSys) 
  (LH:Hash.H with type key=S.lv) 
  (GH:Hash.H with type key=S.gv) =
struct
  open S
  
  let lh_find_default t x a = try LH.find t x with Not_found -> a 
  let gh_find_default t x a = try GH.find t x with Not_found -> a 

  let cons_unique key x xs =
    let xk = key x in
    if List.exists (fun y -> xk = key y) xs
    then xs
    else x::xs

  let solve : (lv*ld) list -> (gv*gd) list -> lv list -> ld LH.t * gd GH.t = fun sl sg iv ->
    let sigma = LH.create 113 in
    let theta = GH.create 113 in
    let vInfl = LH.create 113 in
    let gInfl = GH.create 113 in
    let todo  = LH.create 113 in
    let unsafe = ref [] in
    let workset = ref iv in
    let rec constrainOneVar (x:lv) =
      let rhsides = 
        let notnew = LH.mem sigma in 
        if notnew x then
          let temp = lh_find_default todo x [] in 
          let _ = LH.remove todo x in
            temp
        else begin
          LH.add sigma x (D.bot ());  
          fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system x) ([],0))
        end
      in
      if rhsides=[] then () else begin
        let local_state = ref (D.bot ()) in 
        let constrainOneRHS (f, i) =
          let local_side v state = 
            if not ( D.leq state (D.bot ()) ) then
              let oldstate = lh_find_default sigma v (D.bot ()) in
              begin if not (LH.mem sigma v) then constrainOneVar v end;
              let compls = D.join oldstate state in
                if not (D.leq compls oldstate) then begin
                  let lst = lh_find_default vInfl v [] in
                  LH.replace sigma v compls;
                  unsafe := lst @ !unsafe;
                  LH.remove vInfl v
                end
          in
          let global_side g gstate = 
            if not ( G.leq gstate (G.bot ()) ) then
              let oldgstate = gh_find_default theta g (G.bot ()) in
              let compgs = G.join oldgstate gstate in
                if not (G.leq compgs oldgstate) then begin
                  let lst = gh_find_default gInfl g [] in
                  GH.replace theta g compgs;
                  unsafe := lst @ !unsafe;
                  GH.remove gInfl g
                end
          in
          let nls = f (vEval ((x,f),i)) local_side (gEval ((x,f),i)) global_side in
            local_state := D.join !local_state nls
        in
        List.iter constrainOneRHS rhsides;
        let old_state = lh_find_default sigma x (D.bot ()) in
        let new_val = D.join !local_state old_state in
        if not (D.leq new_val old_state) then begin
          LH.replace sigma x new_val;
          let influenced_vars = ref [] in
          let collectInfluence ((y,f),i) = 
            LH.replace todo y (cons_unique snd (f,i) (lh_find_default todo y []));             
            influenced_vars := y :: !influenced_vars
          in
            List.iter collectInfluence (lh_find_default vInfl x []);
            LH.remove vInfl x;
            List.iter constrainOneVar !influenced_vars
        end 
      end
    and vEval c var =
      constrainOneVar var;
      LH.replace vInfl var (c :: lh_find_default vInfl var []);
      lh_find_default sigma var (D.bot ())
    
    and gEval c glob = 
      GH.replace gInfl glob (c :: gh_find_default gInfl glob []);
      gh_find_default theta glob (G.bot ()) 

    in
      GU.may_narrow := false;
      let add_start (v,d) = 
        LH.add sigma v d;
        let edges = fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system v) ([],0)) in
        LH.add todo v edges;
        workset := v :: !workset
      in
      List.iter add_start sl;
      List.iter (fun (v,g) -> GH.add theta v g) sg;
      while not ([] = !workset) do
        List.iter constrainOneVar !workset;
        workset := [];
        let recallConstraint ((y,f),i) = 
          LH.replace todo y (cons_unique snd (f,i) (lh_find_default todo y []));
          workset := y :: !workset;
        in
          List.iter recallConstraint !unsafe;
          unsafe := []
      done;    
      (sigma, theta)
end

module Make2GGS : Analyses.GenericGlobSolver = Make2