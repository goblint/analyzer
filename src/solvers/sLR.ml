(** The 'slr+' and 'restart' solvers. *)

open Analyses
open Constraints 
open Batteries
open Messages

module type BoolProp  = sig val value : bool end
module      PropTrue  = struct let value = true  end
module      PropFalse = struct let value = false end

module type SolverConf =
sig
  val apply_box : [`localized|`everywhere|`loop_head]
  val restart   : bool
end

(** the box solver *)
module MakeBoxSolver =
  functor (C:SolverConf) -> 
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module VS = Set.Make (S.Var)
  
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
    let add h k v = HM.replace h k (VS.add v (h_find_default h k VS.empty))
    let sub h k = h_find_default h k VS.empty
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
    let insert m x = HM.replace m x ()
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
  
  module O =
  struct
    let m = HM.create 1024
    let add_dep x y =
      HM.replace m x (VS.add y (h_find_default m x VS.empty))
    let mem_deps xs y =
      let found   = ref false in
      let todo    = ref xs in
      let visited = ref VS.empty in
      while VS.cardinal !todo <> 0 do
        if VS.exists (S.Var.equal y) !todo then begin
          found := true;
          todo := VS.empty
        end else begin
          visited := VS.union !todo !visited;
          todo := VS.fold (fun x -> VS.union (VS.diff (h_find_default m x VS.empty) !visited)) !todo VS.empty
        end
      done;
      !found
  end
      
  let infl   = HM.create 1024  
  let wpoint = HM.create 1024 
  let ts     = HM.create 1024 (* (debugging) time-stamps *)
  let time   = ref 0 
  let back   = XY.HPM.create 1024 (* for debugging only *)
                
  let solve box st list = 
    let stable = HM.create 1024 in
    let work   = ref H.empty    in
        
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
    
    let box x v1 v2 = 
      let r = box x v1 v2 in
      if M.tracing then M.tracel "box" "Box operator application: %a \nbefore:%a\nafter:%a\nresult:%a\n\n" S.Var.pretty_trace x S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty r;
      r
    in
    
    let eq old se x get set = 
      let _ = incr time; HM.replace ts x !time in
      let sides = HM.create 10 in
      let do_one (d_in,d_back) f =
        let gets  = ref VS.empty in
        let gets' = ref VS.empty in
        let collect_set x v = 
          h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
        in 
        let collect_get x = 
          let r = get x in
          let _ = gets := 
            let n = List.length (S.system x) in
            if n<2 then 
              VS.union !gets (h_find_default O.m x VS.empty) 
            else 
              VS.add x !gets 
          in          
          let _ = gets' := VS.add x !gets' in
          r
        in 
        let d = f (if C.apply_box=`localized then collect_get else get) collect_set in
        if C.apply_box=`localized then begin 
          if O.mem_deps !gets' x then begin
            VS.iter (fun y -> XY.HPM.add back (y,x) ()) !gets';
            (d_in, S.Dom.join d_back d)
          end else begin 
            VS.iter (O.add_dep x) !gets;
            (S.Dom.join d_in d, d_back)
          end
        end else 
          (d_in, S.Dom.join d_back d)
      in
      let d_in, d_back = List.fold_left do_one (S.Dom.bot (), se) (S.system x) in
      HM.iter set sides;
      HM.clear sides;
      match C.apply_box with
        | `localized when S.Dom.is_bot d_back -> d_in
        | `localized  -> 
            HM.replace wpoint x ();
            (S.Dom.join d_in (box x old d_back))
        | `everywhere -> 
            let _ = L.add infl x x in
            box x old (S.Dom.join d_in d_back)
        | `loop_head  -> 
            if HM.mem wpoint x then 
              let _ = L.add infl x x in
              box x old (S.Dom.join d_in d_back) 
            else 
              S.Dom.join d_in d_back
    in 
        
    let restart x = 
      let (sk,_) = X.get_index x in
      let rec handle_one x =
        let (k,_) = X.get_index x in
        let _ = work := H.insert !work x in
        let _ = P.rem_item stable x in
        if k >= sk then () else
        let _ = X.set_value x (D.bot ()) in
        (*ignore @@ Pretty.printf "  also restarting %d: %a\n" k S.Var.pretty_trace x;*)
        let w = L.sub infl x in
        let _ = L.rem_item infl x in
        VS.iter handle_one w 
      in 
      (*ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x;*)
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      let _ = if C.apply_box=`everywhere || HM.mem wpoint x then L.add infl x x in
      VS.iter handle_one w 
    in 
    
    let rec eval x =
      let (xi,_) = X.get_index x in
      fun y ->
        let (i,nonfresh) = X.get_index y in
        let _ = if i>=xi && C.apply_box=`loop_head then HM.replace wpoint y () in
        let _ = if nonfresh then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                    
    and side x y d = 
      if (C.apply_box=`loop_head) then HM.replace wpoint y ();
      
      let _ = 
        match T.sub T.set y with 
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      let tmp = box x old d in 
     
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
            (*ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
            List.fold_left (fun a z -> D.cup a (XY.get_value (z,x))) a xs

    and solve x = 
      if not (P.has_item stable x) then begin
        incr Goblintutil.evals;
        let _ = P.insert stable x in
        let old = X.get_value x in
        let tmp = eq old (do_side x (S.Dom.bot ())) x (eval x) (side x) in 
        (*let tmp = S.Dom.join (box x old (do_side x (S.Dom.bot ()))) (eq old x (eval x) (side x)) in *)
        (*let tmp = if C.apply_box=`everywhere then box x old tmp else tmp in*)
        if D.eq tmp old then 
          loop (X.get_key x)
        else begin 
          let _ = X.set_value x tmp in
          let rstrt = 
            C.restart &&
            match C.apply_box with
              | `localized | `loop_head -> HM.mem wpoint x && D.leq tmp old
              | `everywhere             -> D.leq tmp old
          in 

          if rstrt then 
            restart x 
          else
            (*let _ = 
              if tracing then trace "solver" "Var changed! %a --\n%avs:\n%a\n\n" S.Var.pretty_trace x S.Dom.pretty tmp S.Dom.pretty old
            in*)
            let w = L.sub infl x in
            let _ = L.rem_item infl x in
            (*let _ = if C.apply_box=`everywhere || HM.mem wpoint x then L.add infl x x in*)
            let h = VS.fold (flip H.insert) w !work in
            let _ = work := h in
                    VS.iter (P.rem_item stable) w;
                    
          loop (X.get_key x) 
        end 
      end
        
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
    X.to_list ()

end

module PhasesSolver0 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module Seq = Generic.SimpleSysConverter (S)
  
  module S1 = 
  struct 
    include Generic.SoundBoxSolverImpl (Seq) (HM)
  end

  module S2 = 
  struct
    include Generic.SoundBoxSolverImpl (Seq) (HM)
  end
  
  let solve box st list = 
    let widen _ x y = S.Dom.widen x (S.Dom.join x y) in
    let narrow _ x y = S.Dom.narrow x y in
    let r1 = S1.solveWithStart widen (HM.create 1024, HM.create 1024) st list in
    let r2 = S2.solveWithStart narrow r1 st list in
    fst r2
end


module PhasesSolver =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = 
  struct 
    include MakeBoxSolver (struct let apply_box = `everywhere let restart = false end) (S) (HM)
  end
  module S2 = 
  struct
    include MakeBoxSolver (struct let apply_box = `everywhere let restart = false end) (S) (HM)
  end
  
  let solve box st list = 
    let _ = S1.solve (fun _ x y -> S.Dom.widen x (S.Dom.join x y)) st list in
    S1.XY.HPM.iter (fun k v -> S2.XY.HPM.add S2.XY.xy k v) S1.XY.xy;
    HM.iter (fun k v -> HM.add S2.X.vals k v) S1.X.vals;
    HM.iter (fun k v -> HM.add S2.T.set k v) S1.T.set;
    let r2 = S2.solve (fun _ x y -> S.Dom.narrow x y) [] list in
    r2
end

module MakeBoxSolverCMP =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = PhasesSolver (S) (HM)
  module S2 = MakeBoxSolver (struct let apply_box = `loop_head let restart = false end) (S) (HM)
  
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
      else if b1 then
        f_le ()
      else if b2 then
        f_gr ()
      else 
        f_uk ()
    in
    HM.iter f r1;
    Printf.printf "eq=%d\tle=%d\tgr=%d\tuk=%d\n" !eq !le !gr !uk;
    r1
end

module MakeBoxSolverCMP2 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = PhasesSolver (S) (HM)
  module S2 = MakeBoxSolver (struct let apply_box = `loop_head let restart = true end) (S) (HM)
  
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
      else if b1 then
        f_le ()
      else if b2 then
        f_gr ()
      else 
        f_uk ()
    in
    HM.iter f r1;
    Printf.printf "eq=%d\tle=%d\tgr=%d\tuk=%d\n" !eq !le !gr !uk;
    r1
end


module MakeRestartSolverCMP =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = MakeBoxSolver (struct let apply_box = `loop_head let restart = true  end)  (S) (HM)
  module S2 = MakeBoxSolver (struct let apply_box = `loop_head let restart = false end) (S) (HM)
  
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
      else if b1 then
        f_le ()
      else if b2 then
        f_gr ()
      else 
        f_uk ()
    in
    HM.iter f r1;
    Printf.printf "eq=%d\tle=%d\tgr=%d\tuk=%d\n" !eq !le !gr !uk;
    r1
end

module MakeWdiffCMP =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = MakeBoxSolver (struct let apply_box = `loop_head  let restart = false end) (S) (HM)
  module S2 = MakeBoxSolver (struct let apply_box = `everywhere let restart = false end) (S) (HM)
  
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
      else if b1 then
        f_le ()
      else if b2 then
        f_gr ()
      else 
        f_uk ()
    in
    HM.iter f r1;
    Printf.printf "eq=%d\tle=%d\tgr=%d\tuk=%d\n" !eq !le !gr !uk;
    r1
end

module PrintInfluence =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = MakeBoxSolver (struct let apply_box = `localized let restart = false end) (S) (HM)
  let solve box x y =
    let ch = Legacy.open_out "test.dot" in
    let r = S1.solve box x y in
    let f k _ =
      let q = if HM.mem S1.wpoint k then " shape=box style=rounded" else "" in
      let s = Pretty.sprint 80 (S.Var.pretty_trace () k) ^ " " ^ string_of_int (try HM.find S1.ts k with Not_found -> 0) in
      ignore (Pretty.fprintf ch "%d [label=\"%s\"%s];\n" (S.Var.hash k) (Goblintutil.escape s) q);
      let f y =
        if S1.XY.HPM.mem S1.back (k,y) then
          ignore (Pretty.fprintf ch "%d -> %d [arrowhead=box style=dashed];\n" (S.Var.hash k) (S.Var.hash y))
        else
          ignore (Pretty.fprintf ch "%d -> %d ;\n" (S.Var.hash k) (S.Var.hash y))
      in 
      S1.VS.iter f (try HM.find S1.infl k with Not_found -> S1.VS.empty)
      ; S1.VS.iter (fun y -> ignore (Pretty.fprintf ch "%d -> %d [constraint=false style=dotted];\n" (S.Var.hash k) (S.Var.hash y))) (S1.h_find_default S1.O.m k S1.VS.empty)
    in
    ignore (Pretty.fprintf ch "digraph G {\nedge [arrowhead=vee];\n");
    HM.iter f r;
    ignore (Pretty.fprintf ch "}\n");
    Legacy.close_out_noerr ch;
    r
end

let _ =
  let module MakeIsGenericIneqBoxSolver : GenericIneqBoxSolver = MakeBoxSolver (struct let apply_box = `everywhere let restart = false end) in
  ()

let _ =
  let module M = GlobSolverFromIneqSolver(MakeBoxSolver (struct let apply_box = `loop_head let restart = false end)) in
  Selector.add_solver ("slr+", (module M : GenericGlobSolver));
  let module M7 = GlobSolverFromIneqSolver(MakeBoxSolver (struct let apply_box = `everywhere let restart = false end)) in
  Selector.add_solver ("new", (module M7 : GenericGlobSolver));
  let module M1 = GlobSolverFromIneqSolver(MakeBoxSolver (struct let apply_box = `loop_head let restart = true end)) in
  Selector.add_solver ("restart", (module M1 : GenericGlobSolver));
  let module M3 = GlobSolverFromIneqSolver(PhasesSolver0) in
  Selector.add_solver ("fwtn", (module M3 : GenericGlobSolver));
  let module M2 = GlobSolverFromIneqSolver(MakeWdiffCMP) in
  Selector.add_solver ("cmpwdiff", (module M2 : GenericGlobSolver));
  let module M4 = GlobSolverFromIneqSolver(MakeRestartSolverCMP) in
  Selector.add_solver ("cmprest", (module M4 : GenericGlobSolver));
  let module M5 = GlobSolverFromIneqSolver(MakeBoxSolverCMP) in
  Selector.add_solver ("cmpfwtn", (module M5 : GenericGlobSolver));
  let module M6 = GlobSolverFromIneqSolver(PrintInfluence) in
  Selector.add_solver ("slr+infl", (module M6 : GenericGlobSolver));
  let module M8 = GlobSolverFromIneqSolver(MakeBoxSolver (struct let apply_box = `localized let restart = false end)) in
  Selector.add_solver ("loc", (module M8 : GenericGlobSolver));
  let module M9 = GlobSolverFromIneqSolver(MakeBoxSolver (struct let apply_box = `localized let restart = true end)) in
  Selector.add_solver ("loc+rest", (module M9 : GenericGlobSolver));
  let module M10 = GlobSolverFromIneqSolver(MakeBoxSolverCMP2) in
  Selector.add_solver ("cmp", (module M10 : GenericGlobSolver));
  
  
(** a copy of the the box solver *)
module MakeBoxSolverWithCalled =
  functor (C:SolverConf) -> 
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module VS = Set.Make (S.Var)

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
    let add h k v = HM.replace h k (VS.add v (h_find_default h k VS.empty))
    let sub h k = h_find_default h k VS.empty
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
    let insert m x = HM.replace m x ()
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
  
  module O =
  struct
    let m = HM.create 1024
    let add_dep x y =
      HM.replace m x (VS.add y (h_find_default m x VS.empty))
    let mem_deps xs y =
      let found   = ref false in
      let todo    = ref xs in
      let visited = ref VS.empty in
      while VS.cardinal !todo <> 0 do
        if VS.exists (S.Var.equal y) !todo then begin
          found := true;
          todo := VS.empty
        end else begin
          visited := VS.union !todo !visited;
          todo := VS.fold (fun x -> VS.union (VS.diff (h_find_default m x VS.empty) !visited)) !todo VS.empty
        end
      done;
      !found
  end
  
  let infl   = HM.create 1024  
  let wpoint = HM.create 1024 
  let back   = XY.HPM.create 1024 (* for debugging only *)
  let called = HM.create 1024
              
  let solve box st list = 
    let stable = HM.create 1024 in
    let work   = ref H.empty    in
      
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
  
    let box x v1 v2 = 
      let r = box x v1 v2 in
      if M.tracing then M.tracel "box" "Box operator application: %a \nbefore:%a\nafter:%a\nresult:%a\n\n" S.Var.pretty_trace x S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty r;
      r
    in
        
  
    let eq old se x get set = 
      let sides = HM.create 10 in
      let do_one (d_in,d_back) f =
        let gets' = ref VS.empty in
        let collect_set x v = 
          h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
        in 
        let collect_get x = 
          let r = get x in
          let _ = gets' := VS.add x !gets' in
          r
        in 
        let d = f (if C.apply_box=`localized then collect_get else get) collect_set in
        if C.apply_box=`localized then begin 
          if O.mem_deps !gets' x then begin
            VS.iter (fun y -> XY.HPM.add back (y,x) ()) !gets';
            HM.replace O.m x (VS.remove x (h_find_default O.m x VS.empty));
            HM.replace wpoint x ();
            (d_in, S.Dom.join d_back d)
          end else begin
            (S.Dom.join d_in d, d_back)
          end
        end else 
          (d_in, S.Dom.join d_back d)
      in
      let d_in, d_back = List.fold_left do_one (S.Dom.bot (), se) (S.system x) in
      HM.iter set sides;
      HM.clear sides;
      match C.apply_box with
        | `localized when S.Dom.is_bot d_back -> d_in
        | `localized  -> 
            let _ = L.add infl x x in
            (S.Dom.join d_in (box x old d_back))
        | `everywhere -> 
            let _ = L.add infl x x in
            box x old (S.Dom.join d_in d_back)
        | `loop_head  -> 
            if HM.mem wpoint x then 
              let _ = L.add infl x x in
              box x old (S.Dom.join d_in d_back) 
            else 
              S.Dom.join d_in d_back
    in 
      
    let restart x = 
      let (sk,_) = X.get_index x in
      let rec handle_one x =
        let (k,_) = X.get_index x in
        let _ = work := H.insert !work x in
        let _ = P.rem_item stable x in
        if k >= sk then () else
        let _ = X.set_value x (D.bot ()) in
        (*ignore @@ Pretty.printf "  also restarting %d: %a\n" k S.Var.pretty_trace x;*)
        let w = L.sub infl x in
        let _ = L.rem_item infl x in
        VS.iter handle_one w 
      in 
      (*ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x;*)
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      let _ = if C.apply_box=`everywhere || (C.apply_box=`loop_head && HM.mem wpoint x) then L.add infl x x in
      VS.iter handle_one w 
    in 
  
    let rec eval x =
      let (xi,_) = X.get_index x in
      fun y ->
        let (i,nonfresh) = X.get_index y in
        let _ = 
          if HM.mem called y then 
            O.add_dep x y 
          else
            VS.iter (O.add_dep x) (h_find_default O.m y VS.empty) 
        in
        let _ = if i>=xi && C.apply_box=`loop_head then HM.replace wpoint y () in
        let _ = if nonfresh then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                  
    and side x y d = 
      if C.apply_box=`loop_head then HM.replace wpoint y ();
    
      let _ = 
        match T.sub T.set y with 
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      let tmp = box x old d in 
   
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
            (*ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
            List.fold_left (fun a z -> D.cup a (XY.get_value (z,x))) a xs


    and solve x = 
      if not (P.has_item stable x) then begin
        incr Goblintutil.evals;
        let _ = P.insert stable x in
        let _ = P.insert called x in
        let old = X.get_value x in
        let tmp = eq old (do_side x (S.Dom.bot ())) x (eval x) (side x) in 
        let _ = P.rem_item called x in
        
        (*let tmp = if C.apply_box=`everywhere then box x old tmp else tmp in*)
        if D.eq tmp old then 
          loop (X.get_key x)
        else begin 
          let _ = X.set_value x tmp in
          let rstrt = 
            C.restart &&
            match C.apply_box with
              | `localized | `loop_head -> HM.mem wpoint x && D.leq tmp old
              | `everywhere             -> D.leq tmp old
          in 

          if rstrt then 
            restart x 
          else
            let w = L.sub infl x in
            let _ = L.rem_item infl x in
            let _ = if C.apply_box=`everywhere || HM.mem wpoint x then L.add infl x x in
            let h = VS.fold (flip H.insert) w !work in
            let _ = work := h in
                    VS.iter (P.rem_item stable) w;
                  
          loop (X.get_key x) 
        end 
      end
      
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
    X.to_list ()

end

module PrintInfluence2 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = MakeBoxSolverWithCalled (struct let apply_box = `localized let restart = false end) (S) (HM)
  let solve box x y =
    let ch = Legacy.open_out "test.dot" in
    let r = S1.solve box x y in
    let f k _ =
      let q = if HM.mem S1.wpoint k then " shape=box style=rounded" else "" in
      let s = Pretty.sprint 80 (S.Var.pretty_trace () k) ^ " " ^ string_of_int (S1.X.get_key k) in
      ignore (Pretty.fprintf ch "%d [label=\"%s\"%s];\n" (S.Var.hash k) (Goblintutil.escape s) q);
      let f y =
        if S1.XY.HPM.mem S1.back (k,y) then
          ignore (Pretty.fprintf ch "%d -> %d [arrowhead=box style=dashed];\n" (S.Var.hash k) (S.Var.hash y))
        else
          ignore (Pretty.fprintf ch "%d -> %d ;\n" (S.Var.hash k) (S.Var.hash y))
      in 
      S1.VS.iter f (try HM.find S1.infl k with Not_found -> S1.VS.empty)
      ; S1.VS.iter (fun y -> ignore (Pretty.fprintf ch "%d -> %d [constraint=false style=dotted];\n" (S.Var.hash k) (S.Var.hash y))) (S1.h_find_default S1.O.m k S1.VS.empty)
    in
    ignore (Pretty.fprintf ch "digraph G {\nedge [arrowhead=vee];\n");
    HM.iter f r;
    ignore (Pretty.fprintf ch "}\n");
    Legacy.close_out_noerr ch;
    r
end

let _ =
  let module M6 = GlobSolverFromIneqSolver(PrintInfluence2) in
  Selector.add_solver ("slr+infl2", (module M6 : GenericGlobSolver));
  

(** the box solver *)
module MakeBoxSolver3 =
  functor (C:SolverConf) -> 
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module VS = Set.Make (S.Var)

  let h_find_option h x =
    try Some (HM.find h x)
    with Not_found -> None

  let h_find_default h x d =
    try HM.find h x 
    with Not_found -> d

  module O =
  struct
    let m = HM.create 1024
    let add_dep x y =
      HM.replace m x (VS.add y (h_find_default m x VS.empty))
    let mem_deps xs y =
      let found   = ref false in
      let todo    = ref xs in
      let visited = ref VS.empty in
      while VS.cardinal !todo <> 0 do
        if VS.exists (S.Var.equal y) !todo then begin
          found := true;
          todo := VS.empty
        end else begin
          visited := VS.union !todo !visited;
          todo := VS.fold (fun x -> VS.union (VS.diff (h_find_default m x VS.empty) !visited)) !todo VS.empty
        end
      done;
      !found
  end
  
  (** Helper module for priority queues. *)
  module H = 
  struct
    let compare x y = 
      let y_in_depx = O.mem_deps (VS.singleton x) y in
      let x_in_depy = O.mem_deps (VS.singleton y) x in
      if y_in_depx && x_in_depy then
        S.Var.compare x y
      else if x_in_depy then 
        1 
      else if y_in_depx then
        -1
      else 
        0
        
    module HeapCompare = 
    struct
      type t = S.Var.t
      let compare = compare
    end

    include Heap.Make (HeapCompare)
    let from_list xs = List.enum xs |> of_enum
    let is_empty x = size x = 0
    let extract_min h = (find_min h, del_min h)
    let insert h k = 
      (*Printf.printf "add %d\n" (X.get_key k);*)
      insert h k 
    let extract_min h =
      let (k,h) = extract_min h in
      (*Printf.printf "removing %d\n" (X.get_key k);*)
      (k,h)
  end
  

  (** Helper module for values and priorities. *)
  module X = 
  struct 
    let vals = HM.create 1024 
  
    let get_value x = h_find_default vals x (S.Dom.bot ())
    let set_value = HM.replace vals
        
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
  
  (** Helper module for influence lists. *)
  module L = 
  struct
    let add h k v = HM.replace h k (VS.add v (h_find_default h k VS.empty))
    let sub h k = h_find_default h k VS.empty
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
    let insert m x = HM.replace m x ()
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
    
  let infl   = HM.create 1024  
  let wpoint = HM.create 1024 
  let ts     = HM.create 1024 (* (debugging) time-stamps *)
  let time   = ref 0 
  let back   = XY.HPM.create 1024 (* for debugging only *)
              
  let solve box st list = 
    let stable = HM.create 1024 in
    let work   = ref H.empty    in
      
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
  
    let box x v1 v2 = 
      let r = box x v1 v2 in
      if M.tracing then M.tracel "box" "Box operator application: %a \nbefore:%a\nafter:%a\nresult:%a\n\n" S.Var.pretty_trace x S.Dom.pretty v1 S.Dom.pretty v2 S.Dom.pretty r;
      r
    in
  
    let eq old se x get set = 
      let _ = incr time; HM.replace ts x !time in
      let sides = HM.create 10 in
      let do_one (d_in,d_back) f =
        let gets  = ref VS.empty in
        let gets' = ref VS.empty in
        let collect_set x v = 
          h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
        in 
        let collect_get x = 
          let r = get x in
          let _ = gets := 
            let n = List.length (S.system x) in
            if n<2 then 
              VS.union !gets (h_find_default O.m x VS.empty) 
            else 
              VS.add x !gets 
          in          
          let _ = gets' := VS.add x !gets' in
          r
        in 
        let d = f (if C.apply_box=`localized then collect_get else get) collect_set in
        if C.apply_box=`localized then begin 
          if O.mem_deps !gets' x then begin
            VS.iter (fun y -> XY.HPM.add back (y,x) ()) !gets';
            (d_in, S.Dom.join d_back d)
          end else begin 
            VS.iter (O.add_dep x) !gets;
            (S.Dom.join d_in d, d_back)
          end
        end else 
          (d_in, S.Dom.join d_back d)
      in
      let d_in, d_back = List.fold_left do_one (S.Dom.bot (), se) (S.system x) in
      HM.iter set sides;
      HM.clear sides;
      match C.apply_box with
        | `localized when S.Dom.is_bot d_back -> d_in
        | `localized  -> 
            HM.replace wpoint x ();
            (S.Dom.join d_in (box x old d_back))
        | `everywhere -> 
            let _ = L.add infl x x in
            box x old (S.Dom.join d_in d_back)
        | `loop_head  -> 
            if HM.mem wpoint x then 
              let _ = L.add infl x x in
              box x old (S.Dom.join d_in d_back) 
            else 
              S.Dom.join d_in d_back
    in 
      
    (*let restart x = 
      let (sk,_) = X.get_index x in
      let rec handle_one x =
        let (k,_) = X.get_index x in
        let _ = work := H.insert !work x in
        let _ = P.rem_item stable x in
        if k >= sk then () else
        let _ = X.set_value x (D.bot ()) in
        (*ignore @@ Pretty.printf "  also restarting %d: %a\n" k S.Var.pretty_trace x;*)
        let w = L.sub infl x in
        let _ = L.rem_item infl x in
        VS.iter handle_one w 
      in 
      (*ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x;*)
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      let _ = if C.apply_box=`everywhere || HM.mem wpoint x then L.add infl x x in
      VS.iter handle_one w 
    in *)
  
    let rec eval x =
      fun y ->
        let _ = if HM.mem X.vals x then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                  
    and side x y d =     
      let _ = 
        match T.sub T.set y with 
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      let tmp = box x old d in 
   
      if not (D.eq tmp old) then begin
        let _ = XY.set_value (x,y) tmp in

        if HM.mem X.vals x then
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
            (*ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
            List.fold_left (fun a z -> D.cup a (XY.get_value (z,x))) a xs

    and solve x = 
      if not (P.has_item stable x) then begin
        incr Goblintutil.evals;
        let _ = P.insert stable x in
        let old = X.get_value x in
        let tmp = eq old (do_side x (S.Dom.bot ())) x (eval x) (side x) in 
        (*let tmp = S.Dom.join (box x old (do_side x (S.Dom.bot ()))) (eq old x (eval x) (side x)) in *)
        (*let tmp = if C.apply_box=`everywhere then box x old tmp else tmp in*)
        if D.eq tmp old then 
          loop x
        else begin 
          let _ = X.set_value x tmp in
          let _ = 
            if tracing then trace "solver" "Var changed! %a --\n%avs:\n%a\n\n" S.Var.pretty_trace x S.Dom.pretty tmp S.Dom.pretty old
          in
          let w = L.sub infl x in
          let _ = L.rem_item infl x in
          (*let _ = if C.apply_box=`everywhere || HM.mem wpoint x then L.add infl x x in*)
          let h = VS.fold (flip H.insert) w !work in
          let _ = work := h in
          VS.iter (P.rem_item stable) w;                  
          loop x
        end 
      end
      
    and loop a =  
      if not (H.is_empty !work) then begin
        let x = H.find_min !work in
        if H.compare x a < 0
        then let _ = work := H.del_min !work in
             let _ = solve x in
                     loop a
      end
    in 
  
    let rec loop () = 
      if not (H.is_empty !work) then begin
        let x = H.find_min !work in
        let _ = work := H.del_min !work in
        let _ = solve x in
        loop ()
      end
    in 
  
    let _ = loop () in 
    X.to_list ()

end


module PrintInfluence3 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = MakeBoxSolver3 (struct let apply_box = `localized let restart = false end) (S) (HM)
  let solve box x y =
    let ch = Legacy.open_out "test.dot" in
    let r = S1.solve box x y in
    let f k _ =
      let q = if HM.mem S1.wpoint k then " shape=box style=rounded" else "" in
      let s = Pretty.sprint 80 (S.Var.pretty_trace () k) ^ " " ^ string_of_int (try HM.find S1.ts k with Not_found -> 0) in
      ignore (Pretty.fprintf ch "%d [label=\"%s\"%s];\n" (S.Var.hash k) (Goblintutil.escape s) q);
      let f y =
        if S1.XY.HPM.mem S1.back (k,y) then
          ignore (Pretty.fprintf ch "%d -> %d [arrowhead=box style=dashed];\n" (S.Var.hash k) (S.Var.hash y))
        else
          ignore (Pretty.fprintf ch "%d -> %d ;\n" (S.Var.hash k) (S.Var.hash y))
      in 
      S1.VS.iter f (try HM.find S1.infl k with Not_found -> S1.VS.empty)
      ; S1.VS.iter (fun y -> ignore (Pretty.fprintf ch "%d -> %d [constraint=false style=dotted];\n" (S.Var.hash k) (S.Var.hash y))) (S1.h_find_default S1.O.m k S1.VS.empty)
    in
    ignore (Pretty.fprintf ch "digraph G {\nedge [arrowhead=vee];\n");
    HM.iter f r;
    ignore (Pretty.fprintf ch "}\n");
    Legacy.close_out_noerr ch;
    r
end

let _ =
  let module M1 = GlobSolverFromIneqSolver(PrintInfluence3) in
  Selector.add_solver ("slr+infl3", (module M1 : GenericGlobSolver));
