(** The 'slr+' and 'restart' solvers. *)

open Analyses
open Constraints 
open Batteries
open TopDown

module type BoolProp  = sig val value : bool end
module      PropTrue  = struct let value = true  end
module      PropFalse = struct let value = false end

(** the box solver *)
module MakeBoxSolver =
  functor (LIM_WP:BoolProp) -> 
  functor (RES:BoolProp) -> 
  functor (LOC_W:BoolProp) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct

  let h_find_option h x =
    try Some (HM.find h x)
    with Not_found -> None

  let h_find_default h x d =
    try HM.find h x 
    with Not_found -> d

  (** Helper module for values and priorities. *)
  module X = 
  struct 
    let keys = HM.create 1024 
    let vals = HM.create 1024 
    let last_key = ref 0
    
    let get_value x = h_find_default vals x (S.Dom.bot ())
    let set_value = HM.replace vals
    
    let get_key x = 
      try
        HM.find keys x 
      with Not_found -> 
        incr Goblintutil.vars;
        last_key := !last_key - 1;
        HM.add keys x !last_key;
        !last_key
                        
    let get_index c = 
      try (HM.find keys c, true) 
      with Not_found -> 
        incr Goblintutil.vars;
        last_key := !last_key - 1; 
        HM.add keys c !last_key;
        (!last_key, false)
      
      let to_list () = vals 
  end  
    
  (** Helper module for values of global contributions. *)
  module XY = 
  struct
    module P = 
    struct
      type t = S.Var.t * S.Var.t
      let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
      let hash (x1,x2) = (S.Var.hash x1 - 800) * S.Var.hash x2 
    end
    module HPM = Hashtbl.Make (P)
    let hpm_find_default h x d =
      try HPM.find h x 
      with Not_found -> d
    
    let xy = HPM.create 1024 

    let get_value x = hpm_find_default xy x (S.Dom.bot ())
    let set_value = HPM.replace xy
  end

  (** Helper module for priority queues. *)
  module H = 
  struct
    module HeapCompare = 
    struct
      type t = S.Var.t
      let compare x y = Int.compare (X.get_key x) (X.get_key y)
    end
  
    include Heap.Make (HeapCompare)
    let from_list xs = List.enum xs |> of_enum
    let is_empty x = size x = 0
    let get_root_key x = find_min x |> X.get_key 
    let extract_min h = (find_min h, del_min h)
    let insert h k = 
      (*Printf.printf "add %d\n" (X.get_key k);*)
      insert h k 
    let extract_min h =
      let (k,h) = extract_min h in
      (*Printf.printf "removing %d\n" (X.get_key k);*)
      (k,h)
  end
    
  (** Helper module for influence lists. *)
  module L = 
  struct
    let add h k v = HM.replace h k (v::h_find_default h k [])
    let sub h k = h_find_default h k []
    let rem_item = HM.remove 
  end
  
  (** Helper module for the stable set and global variable setting deps. *)
  module P = 
  struct 
    let single x = tap (fun s -> HM.add s x ()) (HM.create 10) 
    let rem_item = HM.remove 
    let to_list s = HM.fold (fun x y z -> x :: z ) s []
    let has_item = HM.mem 
    let rem_item = HM.remove
    let insert m = flip (HM.replace m) ()
  end
  
  (** Helper module for variable setting deps. *)
  module T = 
  struct
    let sub = h_find_option 
    let update = HM.replace
    let set    = HM.create 1024 
  end
        
  (** Helper module for the domain. *)
  module D =
  struct
     include S.Dom
     let eq = equal
     let cup = join
     let cap = meet
  end

  let solve box st list =
    let stable = HM.create 1024 in
    let infl   = HM.create 1024 in
    let wpoint = HM.create 1024 in
    let work   = ref H.empty    in
        
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
    let _ = List.iter (fun x -> if (not LIM_WP.value) then L.add infl x x) list in 
    
    let eq x get set = 
	    match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> 
            let sides = HM.create 10 in
            let collect_set x v = 
              h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
            in
            let d = f get collect_set in
            HM.iter set sides;
            HM.clear sides;
            d
    in 
    
    (*let box' x xk y z = D.widen y (D.join y z) in*)
    let box' x xk y z = box x y z  in
    
    let restart x =
      let (sk,_) = X.get_index x in
      let rec handle_one x =
        let (k,_) = X.get_index x in
        let _ = work := H.insert !work x in
        let _ = P.rem_item stable x in
        if k >= sk then () else
          let _ = X.set_value x (D.bot ()) in
          (*ignore @@ Pretty.printf " also restarting %d: %a\n" k S.Var.pretty_trace x;*)
          let w = L.sub infl x in
          let _ = L.rem_item infl x in
          (*let _ = L.add infl x x in *)
          List.iter handle_one w
      in
      (*ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x;*)
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      let _ = if (not LIM_WP.value) || HM.mem wpoint x then L.add infl x x in
      List.iter handle_one w
    in 
    
    let rec eval x =
      let (xi,_) = X.get_index x in
      fun y ->
        let (i,nonfresh) = X.get_index y in
        let _ = if xi <= i then HM.replace wpoint y () in
        let _ = if LOC_W.value && xi <= i then work := H.insert (!work) y in
        let _ = if nonfresh then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                    
    and side x y d = 
      HM.replace wpoint y ();
      
      let _ = 
        match T.sub T.set y with 
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      (* ignore @@ Pretty.printf "key: %a -> %a\nold: %a\n\nd: %a\n\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty old S.Dom.pretty d; *)
      let tmp = box y old d in
      (* let tmp = D.cup old d in *)
      (* let tmp = d in *)
      (* ignore @@ Pretty.printf "tmp: %a\n\n"  S.Dom.pretty tmp; *)
     
      if not (D.eq tmp old) then begin
        let _ = XY.set_value (x,y) tmp in
        let (i,nonfresh) = X.get_index y in

        if nonfresh then
          let _ = P.rem_item stable y in 
          work := H.insert (!work) y
        else 
          solve y
      end                                               

    and do_side x a = 
      match T.sub T.set x with 
        | None -> a
        | Some p -> 
            let xs = P.to_list p in 
            (* ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
            List.fold_left (fun a z -> D.cup a (XY.get_value (z,x))) a xs

    and solve x = 
      if not (P.has_item stable x) then begin
        incr Goblintutil.evals;
        let _ = P.insert stable x in
        let old = X.get_value x in

        let tmp = do_side x (eq x (eval x) (side x)) in 
        let rstrt = RES.value && D.leq tmp old in
        let tmp = if (not LIM_WP.value) || HM.mem wpoint x then box' x (X.get_key x) old tmp else tmp in
        if D.eq tmp old then 
	  let _ = if LOC_W.value then HM.remove wpoint x in
             loop (X.get_key x)
        else begin 
          let _ = X.set_value x tmp in

          if rstrt then 
            restart x 
          else
            let w = L.sub infl x in
            let _ = L.rem_item infl x in
            let _ = if (not LIM_WP.value) || HM.mem wpoint x then L.add infl x x in
            let h = List.fold_left H.insert (!work) w in
            let _ = work := h in
                    List.iter (P.rem_item stable) w;
            let _ = if LOC_W.value then HM.remove wpoint x in
          loop (X.get_key x) 
        end 
      end;
      if LOC_W.value then HM.remove wpoint x
        
    and loop a =  
      if not (H.is_empty (!work)) then begin
        if H.get_root_key (!work) <= a
        then let (x,h) = H.extract_min (!work) in
             let _ = work := h in
             let _ = solve x in
                     loop a
      end
    in 
    
    let rec loop () = 
      if not (H.is_empty (!work)) then begin
        let (x,h) = H.extract_min (!work) in
        let _ = work := h in
        let _ = solve x in
        loop ()
      end
    in 
    
    let _ = loop () in
    let hm_find_default t x a = try HM.find t x with Not_found -> a in
    let reachability xs =
      let reachable = HM.create (HM.length X.vals) in
      let rec one_var x =
        if not (HM.mem reachable x) then begin
          HM.replace reachable x ();
	  match S.system x with
	    | None -> ()
	    | Some x -> one_constaint x
        end
      and one_constaint f =
        ignore (f (fun x -> one_var x; hm_find_default X.vals x (D.bot ())) (fun x _ -> one_var x))
      in
      List.iter one_var xs;
      HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove X.vals x) X.vals
    in
    reachability list;
    X.to_list ()

end

(* This module allows to compare two different inequality box solvers on the
same inequality constraint system *)

module MakeWdiffCMPIneq =
  functor (Sa:GenericIneqBoxSolver) ->
  functor (Sb:GenericIneqBoxSolver) ->
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = Sa (S) (HM)
  module S2 = Sb (S) (HM)
  let solve box st list = 
    let r1 = S1.solve box st list in
    let r2 = S2.solve box st list in
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq(*; Printf.printf "="*) in
    let f_le () = incr le(*; Printf.printf "<"*) in
    let f_gr () = incr gr(*; Printf.printf ">"*) in
    let f_uk () = incr uk(*; Printf.printf "?"*) in
    let f k v1 = 
      let v2 = try HM.find r2 k with Not_found -> S.Dom.bot () in
      let b1 = S.Dom.leq v1 v2 in
      let b2 = S.Dom.leq v2 v1 in
      if b1 && b2 then 
        f_eq ()
      else if b1 then begin
        (* ignore @@ Pretty.printf "less:\nS1:%a\n\nS2:%a\n\ndiff:%a\n%a\n\n" S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty_diff (v2,v1) S.Var.pretty_trace k; *)
        f_le ()
      end else if b2 then begin
        (* ignore @@ Pretty.printf "greater:\nS1:%a\n\nS2:%a\n\ndiff:%a\n%a\n\n" S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty_diff (v1,v2) S.Var.pretty_trace k; *)
        f_gr ()
      end else begin 
        ignore @@ Pretty.printf "uncomparable:\nS1:%a\n\nS2:%a\n\ndiff:%a\n%a\n\n" S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty_diff (v1,v2) S.Var.pretty_trace k;
        f_uk ()
      end
    in
    HM.iter f r1;
    Printf.printf "eq=%d\tle=%d\tgr=%d\tuk=%d\n" !eq !le !gr !uk;
    r1
end

(* This module allows to compare two different equality box solvers on the
same equality constraint system *)

module MakeWdiffCMPEq =
  functor (Sa:GenericEqBoxSolver) ->
  functor (Sb:GenericEqBoxSolver) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = Sa (S) (HM)
  module S2 = Sb (S) (HM)
  let solve box st list = 
    let r1 = S1.solve box st list in
    let r2 = S2.solve box st list in
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq(*; Printf.printf "="*) in
    let f_le () = incr le(*; Printf.printf "<"*) in
    let f_gr () = incr gr(*; Printf.printf ">"*) in
    let f_uk () = incr uk(*; Printf.printf "?"*) in
    let f k v1 = 
      let v2 = try HM.find r2 k with Not_found -> S.Dom.bot () in
      let b1 = S.Dom.leq v1 v2 in
      let b2 = S.Dom.leq v2 v1 in
      if b1 && b2 then 
        f_eq ()
      else if b1 then begin
        (* ignore @@ Pretty.printf "less:\nS1:%a\n\nS2:%a\n\ndiff:%a\n%a\n\n" S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty_diff (v2,v1) S.Var.pretty_trace k; *)
        f_le ()
      end else if b2 then begin
        (* ignore @@ Pretty.printf "greater:\nS1:%a\n\nS2:%a\n\ndiff:%a\n%a\n\n" S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty_diff (v1,v2) S.Var.pretty_trace k; *)
        f_gr ()
      end else begin
        ignore @@ Pretty.printf "uncomparable:\nS1:%a\n\nS2:%a\n\ndiff:%a\n%a\n\n" S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty_diff (v1,v2) S.Var.pretty_trace k;
        f_uk ()
      end
    in
    HM.iter f r1;
    Printf.printf "eq=%d\tle=%d\tgr=%d\tuk=%d\n" !eq !le !gr !uk;
    r1
end

module MakeIneqSolver (Sol: GenericEqBoxSolver) =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  include Sol (Generic.SimpleSysConverter (S)) (HM)
end

let _ =
  let module SLR1 = MakeIneqSolver ( MakeBoxSolver (PropFalse) (PropFalse) (PropFalse) ) in
  let module SLR2 = MakeIneqSolver ( MakeBoxSolver (PropTrue) (PropFalse) (PropFalse) ) in
  let module SLR3 = MakeIneqSolver( MakeBoxSolver (PropTrue) (PropFalse) (PropTrue) ) in
  let module SLR4 = MakeIneqSolver ( MakeBoxSolver (PropTrue) (PropTrue) (PropTrue) ) in
  let module SLR2res = MakeIneqSolver ( MakeBoxSolver (PropTrue) (PropTrue) (PropFalse) ) in
  let module CMP2vs1 = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR2) (SLR1) ) in
  Selector.add_solver ("cmp-slr2vs1", (module CMP2vs1: GenericGlobSolver));
  let module CMP3vs2 = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR3) (SLR2) ) in
  Selector.add_solver ("cmp-slr3vs2", (module CMP3vs2: GenericGlobSolver));
  let module CMP4vs3 = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR4) (SLR3) ) in
  Selector.add_solver ("cmp-slr4vs3", (module CMP4vs3: GenericGlobSolver));
  let module CMP2resvs2 = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR2res) (SLR2) ) in
  Selector.add_solver ("cmp-slr2resvs2", (module CMP2resvs2: GenericGlobSolver));
  let module CMP3vs2res = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR3) (SLR2res) ) in
  Selector.add_solver ("cmp-slr3vs2res", (module CMP3vs2res: GenericGlobSolver));
  let module CMPTD2vsSLR3 = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (TD2) (SLR3) ) in
  Selector.add_solver ("cmp-td2vsslr3", (module CMPTD2vsSLR3: GenericGlobSolver));
  let module SLR1solve = GlobSolverFromIneqSolver (SLR1) in
  Selector.add_solver ("slr1", (module SLR1solve: GenericGlobSolver)); 
  let module SLR2solve = GlobSolverFromIneqSolver (SLR2) in
  Selector.add_solver ("slr2", (module SLR2solve: GenericGlobSolver)); 
  let module SLR3solve = GlobSolverFromIneqSolver (SLR3) in
  Selector.add_solver ("slr3", (module SLR3solve: GenericGlobSolver)); 
  let module SLR4solve = GlobSolverFromIneqSolver (SLR4) in
  Selector.add_solver ("slr4", (module SLR4solve: GenericGlobSolver)); 
  let module SLR1new = SLR.MakeBoxSolver (struct let apply_box = `everywhere let restart = false end) in
  let module SLR2new = SLR.MakeBoxSolver (struct let apply_box = `loop_head let restart = false end) in
  let module SLR2resnew = SLR.MakeBoxSolver (struct let apply_box = `everywhere let restart = true end) in
  let module CMP1vs1new = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR1) (SLR1new) ) in
  let module CMP2vs2new = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR2) (SLR2new) ) in
  let module CMP2resvs2resnew = GlobSolverFromIneqSolver ( MakeWdiffCMPIneq (SLR2res) (SLR2resnew) ) in
  Selector.add_solver ("cmp-slr1vs1new", (module CMP1vs1new : GenericGlobSolver));
  Selector.add_solver ("cmp-slr2vs2new", (module CMP2vs2new : GenericGlobSolver));
  Selector.add_solver ("cmp-slr2resvs2resnew", (module CMP2resvs2resnew : GenericGlobSolver));
  let module SLR2newsolve = GlobSolverFromIneqSolver (SLR2new) in
  Selector.add_solver ("slr2new", (module SLR2newsolve: GenericGlobSolver)); 
  let module SLR1Eq = MakeBoxSolver (PropFalse) (PropFalse) (PropFalse) in
  let module SLR2Eq = MakeBoxSolver (PropTrue) (PropFalse) (PropFalse) in
  let module SLR3Eq = MakeBoxSolver (PropTrue) (PropFalse) (PropTrue) in
  let module SLR4Eq = MakeBoxSolver (PropTrue) (PropTrue) (PropTrue) in
  let module CMPEq2vs1 = GlobSolverFromEqSolver ( MakeWdiffCMPEq (SLR2Eq) (SLR1Eq) ) in
  let module CMPEq3vs2 = GlobSolverFromEqSolver ( MakeWdiffCMPEq (SLR3Eq) (SLR2Eq) ) in
  let module CMPEq4vs3 = GlobSolverFromEqSolver ( MakeWdiffCMPEq (SLR4Eq) (SLR3Eq) ) in
  Selector.add_solver ("cmpeq-slr2vs1", (module CMPEq2vs1: GenericGlobSolver));
  Selector.add_solver ("cmpeq-slr3vs2", (module CMPEq3vs2: GenericGlobSolver));
  Selector.add_solver ("cmpeq-slr4vs3", (module CMPEq4vs3: GenericGlobSolver));
  let module SLR1EqSolve = GlobSolverFromEqSolver (SLR1Eq) in 
  let module SLR2EqSolve = GlobSolverFromEqSolver (SLR2Eq) in 
  let module SLR3EqSolve = GlobSolverFromEqSolver (SLR3Eq) in 
  let module SLR4EqSolve = GlobSolverFromEqSolver (SLR4Eq) in 
  Selector.add_solver ("slr1eq", (module SLR1EqSolve: GenericGlobSolver));
  Selector.add_solver ("slr2eq", (module SLR2EqSolve: GenericGlobSolver));
  Selector.add_solver ("slr3eq", (module SLR3EqSolve: GenericGlobSolver));
  Selector.add_solver ("slr4eq", (module SLR4EqSolve: GenericGlobSolver));

