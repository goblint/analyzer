open GobConfig 
open Messages
open Progress
open Pretty

module GU = Goblintutil

module Make2
  (S:Analyses.GlobConstrSys) 
  (LH:Hash.H with type key=S.LVar.t) 
  (GH:Hash.H with type key=S.GVar.t) =
struct
  open S
  
  let lh_find_default t x a = try LH.find t x with Not_found -> a 
  let gh_find_default t x a = try GH.find t x with Not_found -> a 

  let cons_unique key x xs =
    let xk = key x in
    if List.exists (fun y -> xk = key y) xs
    then xs
    else x::xs

  let solve : (LVar.t*D.t) list -> (GVar.t*G.t) list -> LVar.t list -> D.t LH.t * G.t GH.t = fun sl sg iv ->
    let sigma = LH.create 113 in
    let theta = GH.create 113 in
    let vInfl = LH.create 113 in
    let gInfl = GH.create 113 in
    let todo  = LH.create 113 in
    let unsafe = ref [] in
    let workset = ref iv in
    let rec constrainOneVar (x:LVar.t) =
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
              if tracing then tracel "theta" "Value of \"%a\" is non-bottom: %a\n" GVar.pretty_trace g G.pretty_diff (gstate, G.bot ());
              let oldgstate = gh_find_default theta g (G.bot ()) in
              let compgs = G.join oldgstate gstate in
                if not (G.leq compgs oldgstate) then begin
                  let lst = gh_find_default gInfl g [] in
                  if tracing then tracel "theta" "Replacing value of variable \"%a\" in globals.\n" GVar.pretty_trace g;
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
      if not (GH.mem theta glob) then begin
        if tracing then tracel "theta" "Adding variable \"%a\" to globals!\n" GVar.pretty_trace glob;
        GH.replace theta glob (G.bot ())
      end;
      GH.replace gInfl glob (c :: gh_find_default gInfl glob []);
      GH.find theta glob 

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