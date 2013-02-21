open Batteries
open GobConfig
open Analyses

(** Convert a an [IneqConstrSys] into an equation system by joining all right-hand sides. *)
module SimpleSysConverter (S:IneqConstrSys) 
  : EqConstrSys 
  with type v = S.v
   and type d = S.d
   and module Var = S.Var
   and module Dom = S.Dom
   =
struct
  type v = S.v
  type d = S.d

  module Var = S.Var
  module Dom = S.Dom
  
  let box _ = S.Dom.join (* ignore the original operator *)
  
  let system x = 
    match S.system x with
      | [] -> None
      | r::rs -> Some (fun get set -> List.fold_left (fun d r' -> Dom.join d (r' get set)) (r get set) rs)
end 

(* move this to some other place! *)
module ExtendInt (B:Analyses.VarType) : Analyses.VarType with type t = B.t * int =
struct
  type t = B.t * int
  let compare ((u1,u2):t) (v1,v2) =
    match Pervasives.compare u2 v2 with
      | 0 -> B.compare u1 v1
      | n -> n
  let equal ((u1,u2):t) (v1,v2) = u2=v2 && B.equal u1 v1
  let category (u,_) = B.category u
  let hash (u,v) = B.hash u + 131233 * v
  let pretty_trace () (u,v:t) =
    Pretty.dprintf "(%a,%d)" B.pretty_trace u v
    
  let line_nr (n,_) = B.line_nr n 
  let file_name (n,_) = B.file_name n 
  let description (n,_) = B.description n 
  let context () (c,_) = B.context () c 
  let loopSep (n,_) = B.loopSep n
end
  

(** Convert a an [IneqConstrSys] into an equation system. *)
module NormalSysConverter (S:IneqConstrSys) 
  : sig include EqConstrSys val conv : S.v -> (S.v * int) end
  with type v = S.v * int
   and type d = S.d
   and module Var = ExtendInt (S.Var)
   and module Dom = S.Dom
   =
struct
  type v = S.v * int
  type d = S.d

  module Var = ExtendInt (S.Var)
  module Dom = S.Dom
  
  let box (x,n) = 
    if n>0 || (n=0 && 1=List.length (S.system x)) then S.box x else Dom.join

  let conv x = 
    match S.system x with
      | [] | [_] -> (x,0)
      | _ -> (x,-1)
    
  let system (x,n) : ((v -> d) -> (v -> d -> unit) -> d) option = 
    let fold_left1 f xs =
      match xs with
        | [] -> failwith "You promised!!!"
        | x::xs -> List.fold_left f x xs
    in
    match S.system x with
      | []           -> None
      | [f] when n=0 -> Some (fun get set -> f (get -| conv) (set -| conv))
      | xs when n=(-1) -> 
          let compute get set =
            fold_left1 Dom.join (List.mapi (fun n _ -> get (x,n)) xs)
          in
          Some compute
      | xs -> 
        try Some (fun get set -> List.at xs n (get -| conv) (set -| conv))
        with Invalid_argument _ -> None 
end 


module SolverStats (S:EqConstrSys) =
struct
  open S
  open Messages
  
  module GU = Goblintutil

  let vars = ref 0
  let evals = ref 0
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

  let start_event () = ()
  let stop_event () = ()
  
  let new_var_event x = 
    vars := !vars + 1;
    if tracing
    then trace "sol" "New %a\n" Var.pretty_trace x

  let get_var_event x = 
    if full_trace
    then trace "sol" "Querying %a\n" Var.pretty_trace x
    
    
  let eval_rhs_event x = 
    if full_trace
    then trace "sol" "(Re-)evaluating %a\n" Var.pretty_trace x;
    evals := !evals + 1;
    if (get_bool "dbg.solver-progress") then (incr stack_d; print_int !stack_d; flush stdout)
    
  let update_var_event x o n = 
    if tracing then increase x;
    if full_trace || ((not (Dom.is_bot o)) && is_some !max_var && Var.equal (from_some !max_var) x) then begin
      if tracing then tracei "sol" "(%d) Update to %a.\n" !max_c Var.pretty_trace x;
      if tracing then traceu "sol" "%a\n\n" Dom.pretty_diff (n, o)
    end
    
end

(** use this if your [box] is [join] *)
module DirtyBoxSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  include SolverStats (S)

  let h_find_default h x d =
    try H.find h x 
    with Not_found -> d

  let solve box xs vs = 
    (* the stabile "set" *)
    let stbl = H.create 1024 in
    (* the influence map *)
    let infl = H.create 1024 in
    (* the solution map *)
    let sol  = H.create 1024 in
    
    (* solve the variable [x] *)
    let rec solve_one x = 
      (* solve [x] only if it is not stable *)
      if not (H.mem stbl x) then begin
        (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
        if not (H.mem sol x) then (new_var_event x; H.add sol x (S.Dom.bot ()));
        (* mark [x] stable *)
        H.replace stbl x ();
        (* set the new value for [x] *)
        eval_rhs_event x;
        Option.may (fun f -> set x (f (eval x) set)) (S.system x)
      end
      
    (** return the value for [y] and mark its influence on [x] *)
    and eval x y =
      (* solve variable [y] *)
      get_var_event y;
      solve_one y;
      (* add that [x] will be influenced by [y] *)
      H.replace infl y (x :: h_find_default infl y []);
      (* return the value for [y] *)
      H.find sol y 
      
    and set x d =
      (* solve variable [y] if it has not been seen before *)
      if not (H.mem sol x) then solve_one x;
      (* do nothing if we have stabilized [x] *)
      let oldd = H.find sol x in
      let newd = box x oldd d in
      update_var_event x oldd newd;
      if not (S.Dom.equal oldd newd) then begin
        (* set the new value for [x] *)
        H.replace sol x newd;
        (* mark dependencies unstable *)
        let deps = h_find_default infl x [] in
        List.iter (H.remove stbl) deps;
        (* remove old influences of [x] -- they will be re-generated if still needed *)
        H.remove infl x;
        (* solve all dependencies *)
        solve_all deps        
      end
      
    (* solve all elements of the list *)
    and solve_all xs = 
      List.iter solve_one xs
    in
    
    (* solve interesting variables and then return the produced table *)
    start_event ();     
    List.iter (fun (k,v) -> set k v) xs;
    solve_all vs; 
    stop_event (); 
    sol
end

(* use this if you do widenings & narrowings (but no narrowings for globals) *)
module SoundBoxSolverImpl =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  include SolverStats (S)
  
  let h_find_default h x d =
    try H.find h x 
    with Not_found -> d
    
  let solveWithStart box (ht,hts) xs vs = 
    (* the stabile "set" *)
    let stbl = H.create 1024 in
    (* the influence map *)
    let infl = H.create 1024 in
    (* the solution map  *)
    let sol  = ht (*H.create 1024*) in
    (* the side-effected solution map  *)
    let sols = hts(*H.create 1024*) in
    (* the called set *)
    let called = H.create 1024 in
    
    (* solve the variable [x] *)
    let rec solve_one x = 
      (* solve [x] only if it is not stable *)
      if not (H.mem stbl x) then begin
        (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
        if not (H.mem sol x) then (new_var_event x; H.add sol x (S.Dom.bot ()));
        (* mark [x] stable *)
        H.replace stbl x ();
        (* mark [x] called *)
        H.replace called x ();
        (* set the new value for [x] *)
        eval_rhs_event x;
        let set_x d = if H.mem called x then set x d else () in
        Option.may (fun f -> set_x (f (eval x) side)) (S.system x);
        (* remove [x] from called *)
        H.remove called x        
      end
      
    (** return the value for [y] and mark its influence on [x] *)
    and eval x y =
      (* solve variable [y] *)
      get_var_event y;
      solve_one y;
      (* add that [x] will be influenced by [y] *)
      H.replace infl y (x :: h_find_default infl y []);
      (* return the value for [y] *)
      H.find sol y
    
    (* this is the function we give to [S.system] *)
    and side x d =
      (* accumulate all side-effects in [sols] *)
      let nd = S.Dom.join d (h_find_default sols x (S.Dom.bot ())) in
      H.replace sols x nd;
      (* do the normal writing operation with the accumulated value *)
      set x nd
      
    and set x d =
      (* solve variable [y] if it has not been seen before *)
      if not (H.mem sol x) then solve_one x;
      (* do nothing if we have stabilized [x] *)
      let oldd = H.find sol x in
      (* compute the new value *)
      let newd = box x oldd (S.Dom.join d (h_find_default sols x (S.Dom.bot ()))) in
      if not (S.Dom.equal oldd newd) then begin
        update_var_event x oldd newd;
        (* set the new value for [x] *)
        H.replace sol x newd;
        (* mark dependencies unstable *)
        let deps = h_find_default infl x [] in
        List.iter (H.remove stbl) deps;
        (* remove old influences of [x] -- they will be re-generated if still needed *)
        H.remove infl x;
        H.replace infl x [x];
        if full_trace
        then Messages.trace "sol" "Need to review %d deps.\n" (List.length deps);
        (* solve all dependencies *)
        solve_all deps        
      end
      
    (* solve all elements of the list *)
    and solve_all xs = 
      List.iter solve_one xs
    in
    
    (* solve interesting variables and then return the produced table *)
    start_event (); 
    List.iter (fun (k,v) -> side k v) xs;
    solve_all vs; stop_event (); 
    sol, sols
    
  (** the solve function *)
  let solve box xs ys = solveWithStart box (H.create 1024, H.create 1024) xs ys |> fst
end

module SoundBoxSolver : GenericEqBoxSolver = SoundBoxSolverImpl



(* use this if you do widenings & narrowings for globals *)
module PreciseSideEffectBoxSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  include SolverStats (S)
  
  let h_find_default h x d =
    try H.find h x 
    with Not_found -> d
  
  module VM = Map.Make (S.Var)
  module VS = Set.Make (S.Var)

  let solve box xs vs = 
    (* the stabile "set" *)
    let stbl  = H.create 1024 in
    (* the influence map *)
    let infl  = H.create 1024 in
    (* the solution map  *)
    let sol   = H.create 1024 in
    (* the side-effected solution map  *)
    let sols  = H.create 1024 in
    (* the side-effected solution map  *)
    let sdeps = H.create 1024 in
    (* the side-effected solution map  *)
    let called = H.create 1024 in
    
    (* solve the variable [x] *)
    let rec solve_one x = 
      (* solve [x] only if it is not stable *)
      if not (H.mem stbl x) then begin
        (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
        if not (H.mem sol x) then (new_var_event x; H.add sol x (S.Dom.bot ()));
        (* mark [x] stable *)
        H.replace stbl x ();
        (* mark [x] called *)
        H.replace called x ();
        (* remove side-effected values *)
        H.remove sols x;
        (* set the new value for [x] *)
        eval_rhs_event x;
        Option.may (fun f -> set x (f (eval x) (side x))) (S.system x);
        (* remove [x] from called *)
        H.remove called x        
      end
      
    (** return the value for [y] and mark its influence on [x] *)
    and eval x y =
      (* solve variable [y] *)
      get_var_event y;
      solve_one y;
      (* add that [x] will be influenced by [y] *)
      H.replace infl y (x :: h_find_default infl y []);
      (* return the value for [y] *)
      H.find sol y
    
    (* this is the function we give to [S.system] *)
    and side y x d =
      (* mark that [y] has a side-effect to [x] *)
      H.replace sdeps x (VS.add y (h_find_default sdeps x VS.empty));
      (* save the value in [sols] *)
      let om = h_find_default sols y VM.empty in
      let nm = VM.modify_def (S.Dom.bot ()) x (S.Dom.join d) om in
      H.replace sols y nm;
      (* do the normal writing operation with the accumulated value *)
      set x d
      
    and set x d =
      (* solve variable [y] if it has not been seen before *)
      if not (H.mem sol x) then solve_one x;
      (* do nothing if we have stabilized [x] *)
      let oldd = H.find sol x in
      (* accumulate all side-effects in [sols] *)
      let find_join_sides z d = 
        try S.Dom.join d (VM.find x (H.find sols z))
        with Not_found -> d
      in
      let newd = box x oldd (VS.fold find_join_sides (h_find_default sdeps x VS.empty) d) in
      update_var_event x oldd newd;
      if not (S.Dom.equal oldd newd) then begin
        (* set the new value for [x] *)
        H.replace sol x newd;
        (* mark dependencies unstable *)
        let deps = h_find_default infl x [] in
        List.iter (H.remove stbl) deps;
        (* remove old influences of [x] -- they will be re-generated if still needed *)
        H.remove infl x;
        (* solve all dependencies *)
        solve_all deps        
      end
      
    (* solve all elements of the list *)
    and solve_all xs = 
      List.iter solve_one xs
    in
    
    (* solve interesting variables and then return the produced table *)
    start_event (); 
    List.iter (fun (k,v) -> side k k v) xs;
    solve_all vs; 
    stop_event ();
    sol
end

module CousotNonBoxSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  module SW =
  struct
    include S
    let box v x y = 
      if (S.Var.loopSep v) 
      then S.Dom.widen x (S.Dom.join x y)
      else S.Dom.join x y
  end
  module SolveW = SoundBoxSolverImpl (SW) (H)
  
  module SN =
  struct
    include S
    let box v x y = S.Dom.narrow x y
        
  end
  module SolveN = SoundBoxSolverImpl (SN) (H)
  
  let solve box xs vs = 
    ignore (Pretty.printf "Widening ...\n");
    let wsol = SolveW.solveWithStart box (H.create 1024, H.create 1024) xs vs in
    ignore (Pretty.printf "Narrowing ...\n");
    SolveN.solveWithStart box wsol xs vs |> fst
end

(** its the best *)
module HelmutBoxSolver (*: GenericEqBoxSolver*) =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct

  let h_find_option h x =
    try Some (HM.find h x)
    with Not_found -> None

  let h_find_default h x d =
    try HM.find h x 
    with Not_found -> d

  module X = 
  struct 
    let keys = HM.create 1024 
    let vals = HM.create 1024 
    let last_key = ref 0
    
    let get_value = S.Dom.bot () |> flip (h_find_default vals) 
    let set_value = HM.replace vals
    
    let get_key x = 
      try
        HM.find keys x 
      with Not_found -> 
        last_key := !last_key - 1;
        HM.add keys x !last_key;
        !last_key
                        
    let get_index c = 
      try (HM.find keys c, true) 
      with Not_found -> 
        last_key := !last_key - 1; 
        HM.add keys c !last_key;
        (!last_key, false)
      
      let to_list () = vals 
  end  
    
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

    let get_value = S.Dom.bot () |> flip (hpm_find_default xy) 
    let set_value = HPM.replace xy
  end
                (* contains variable assignment for pairs *)

  module HeapCompare = 
  struct
    type t = S.Var.t
    let compare x y = Pervasives.compare (X.get_key x) (X.get_key y)
  end
  
  (*module H = 
  struct
    module H' = Heap.Make (HeapCompare)
    module S  = Set.IntSet
    
    type t = H'.t * S.t
    
    let from_list xs = (List.enum xs |> H'.of_enum, List.enum xs |> Enum.map X.get_key |> S.of_enum)
    let is_empty (_,x) = S.is_empty x
    let get_root_key (x,_) = H'.find_min x |> X.get_key 
    let extract_min (h,s) = 
      let mn = H'.find_min h in 
      (mn, (H'.del_min h, S.remove (X.get_key mn) s))
    let empty = (H'.empty, S.empty)
    let insert (h1,s1) x = 
      if S.mem (X.get_key x) s1
      then (h1,s1) 
      else (H'.insert h1 x, S.add (X.get_key x) s1) 
    let add k v = insert v k
    let merge (h1,s1) (h2,s2) = (H'.merge h1 h2, S.union s1 s2)
  end*)
  module H = 
  struct
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
  module L = 
  struct
    let add h k v = HM.replace h k (v::h_find_default h k [])
    let sub h k = h_find_default h k []
    let rem_item = HM.remove 
  end
  module P = 
  struct 
    let single x = tap (fun s -> HM.add s x ()) (HM.create 10) 
    let rem_item = HM.remove 
    let to_list s = HM.fold (fun x y z -> x :: z ) s []
    let has_item = HM.mem 
    let rem_item = HM.remove
    let insert m = flip (HM.replace m) ()
  end
  
  module T = 
  struct
    let sub = h_find_option 
    let update = HM.replace
  end
        
  module D =
  struct
     include S.Dom
     let eq = equal
     let cup = join
     let cap = meet
  end
        
        let stable = HM.create 1024
        let infl = HM.create 1024
        let set = HM.create 1024
        let work = ref H.empty
        
        let solve box eq list = 
                        
                          let _ = work := H.merge (H.from_list list) !work 
                          
                          in let _ = List.iter (fun x -> L.add infl x x) list 

                          in let rec eval x y =
                                          let (i,nonfresh) = X.get_index y in
                                          let _ = if nonfresh then ()
                                                  else solve y in
                                          let _ = L.add infl y x in
                                          X.get_value y
                        
                          and side x y d = 
                                            (*ignore (Pretty.printf "side effect %a to (%a,%a)\n\n" D.pretty d S.Var.pretty_trace x S.Var.pretty_trace y);*)

                                           let _ = match T.sub set y
                                                  with None -> T.update set y (P.single x)
                                                  | Some p -> P.insert p x
                                           in 

                                           let old = XY.get_value (x,y)
                                           in let tmp = box y old d
                                           in if  D.eq tmp old then ()
                                              else let _ = XY.set_value (x,y) tmp in
                                                   let (i,nonfresh) = X.get_index y in

                                                   if nonfresh then
                                                           let _ = P.rem_item stable y in 
                                                          work := H.insert (!work) y
                                                  else solve y
                                                               

                          and do_side x a = match T.sub set x 
                                          with None -> a
                                          | Some p -> let list = P.to_list p
                                                  in (*ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
                                                    List.fold_left (
                                                          fun a z ->
                                                          D.cup a (XY.get_value (z,x))
                                                                  ) a list

                          and solve x   = if P.has_item stable x then ()
                                  else    let _ = P.insert stable x in
                                          let old = X.get_value x in
                                          let tmp = do_side x (eq x (eval x) (side x)) in 
                                          let tmp = box x old tmp in
                                          if D.eq tmp old then loop (X.get_key x)
                                          else begin 
                                                  let _ = X.set_value x tmp in
					
                                                  (*let _ = Pretty.printf "%a : %a\n" S.Var.pretty_trace x D.pretty tmp in*)
					
                                                  let w = L.sub infl x in
                                                  let _ = L.rem_item infl x in
                                                  let _ = L.add infl x x in
                                                  let h = List.fold_left H.insert (!work) w in
                                                  let _ = work := h in
                                                          List.iter (P.rem_item stable) w;
                                                          loop (X.get_key x) end
        
                          and loop a =  if H.is_empty (!work) then ()
                                                  else if H.get_root_key (!work) <= a
                                                  then let (x,h) = H.extract_min (!work) in
                                                       let _ = work := h in
                                                       let _ = solve x in
                                                               loop a


                          in let rec loop () =  if H.is_empty (!work) then ()
                                        else      let (x,h) = H.extract_min (!work) in
                                                  let _ = work := h in
                                                  let _ = solve x in
                                                          loop ()

                          in let _ = loop ()
                          in X.to_list ()
    

  let solve box st x = 
    let sys x get set = 
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
    List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update set x (P.single x)) st;
    solve box sys x
    
  let box v x y = 
    if S.Var.loopSep v
    then (if D.leq y x then D.narrow x y else D.widen x (D.join x y))
    else (if D.leq y x then y else D.join x y)
end

module CompareBoxSolvers' : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  module Solve1 = CousotNonBoxSolver (S) (H)  
  module Solve2 = HelmutBoxSolver    (S) (H)
  
  let equal = ref 0
  let smaller = ref 0
  let bigger = ref 0
  let uncomp = ref 0
  let bla = ref true
  
  let print x y = 
    if !bla then begin
      ignore (Pretty.printf "\n%a\n\n%a\n\n" S.Dom.pretty x S.Dom.pretty y);
      bla := false
    end
  
  let report s1 s2 k =
    try
      let e1 = H.find s1 k in
      try
        let e2 = H.find s2 k in
        match S.Dom.leq e1 e2, S.Dom.leq e2 e1 with
          | true , true  -> Printf.printf "="; incr equal
          | true , false -> Printf.printf "<"; incr smaller
          | false, true  -> Printf.printf ">"; incr bigger
          | false, false -> Printf.printf "?"; incr uncomp
      with Not_found -> Printf.printf ">"; incr bigger
    with Not_found -> Printf.printf "<"; incr smaller
  
  let solve box xs vs = 
    let module S = Set.Make (S.Var) in
    let s1 = Solve1.solve box xs vs in
    let s2 = Solve2.solve box xs vs in
    let s = ref S.empty in
    H.iter (fun k v -> s := S.add k !s) s1;
    H.iter (fun k v -> s := S.add k !s) s2;
    S.iter (report s1 s2) !s;
    Printf.printf "\nequal=%d\tsmaller=%d\tbigger=%d\tuncomp=%d\ttotal=%d\n" !equal !smaller !bigger !uncomp (S.cardinal !s);
    s1
end

module CompareBoxSolvers : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  module Solver0 = HelmutBoxSolver (S) (H)
  module Solver1 = HelmutBoxSolver (S) (H)
  
  let equal = ref 0
  let smaller = ref 0
  let bigger = ref 0
  let uncomp = ref 0
  let bla = ref true
  
  let print x y = 
    if !bla then begin
      ignore (Pretty.printf "\n%a\n\n%a\n\n" S.Dom.pretty x S.Dom.pretty y);
      bla := false
    end
  
  let report s1 s2 k =
    try
      let e1 = H.find s1 k in
      try
        let e2 = H.find s2 k in
        match S.Dom.leq e1 e2, S.Dom.leq e2 e1 with
          | true , true  -> Printf.printf "="; incr equal
          | true , false -> Printf.printf "<"; incr smaller
          | false, true  -> Printf.printf ">"; incr bigger
          | false, false -> Printf.printf "?"; incr uncomp
      with Not_found -> Printf.printf ">"; incr bigger
    with Not_found -> Printf.printf "<"; incr smaller
  
  let solve box xs vs = 
    let box1 v x y = if S.Var.loopSep v then S.Dom.widen x (S.Dom.join x y) else S.Dom.join x y in
    let _ = Solver0.solve box1 xs vs in
    let box2 v x y = S.Dom.narrow x y in
    let _ = H.iter (fun k v -> Solver0.work := Solver0.H.add k !Solver0.work) Solver0.stable in
    let s1 = Solver0.solve box2 [] [] in
    let s2 = Solver1.solve box xs vs in
    let module S = Set.Make (S.Var) in
    let s = ref S.empty in
    H.iter (fun k v -> s := S.add k !s) s1;
    H.iter (fun k v -> s := S.add k !s) s2;
    S.iter (report s1 s2) !s;
    Printf.printf "\nequal=%d\tsmaller=%d\tbigger=%d\tuncomp=%d\ttotal=%d\n" !equal !smaller !bigger !uncomp (S.cardinal !s);
    s1
end

module CompareWPoints : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  module Solver0 = HelmutBoxSolver (S) (H)
  module Solver1 = HelmutBoxSolver (S) (H)
  
  let equal = ref 0
  let smaller = ref 0
  let bigger = ref 0
  let uncomp = ref 0
  let bla = ref true
  
  let print x y = 
    if !bla then begin
      ignore (Pretty.printf "\n%a\n\n%a\n\n" S.Dom.pretty x S.Dom.pretty y);
      bla := false
    end
  
  let report s1 s2 k =
    try
      let e1 = H.find s1 k in
      try
        let e2 = H.find s2 k in
        match S.Dom.leq e1 e2, S.Dom.leq e2 e1 with
          | true , true  -> Printf.printf "="; incr equal
          | true , false -> Printf.printf "<"; incr smaller
          | false, true  -> Printf.printf ">"; incr bigger
          | false, false -> Printf.printf "?"; incr uncomp
      with Not_found -> Printf.printf ">"; incr bigger
    with Not_found -> Printf.printf "<"; incr smaller
  
  let solve box xs vs = 
    set_bool "exp.back_loop_sep" false;
    let s1 = Solver0.solve box xs vs in
    set_bool "exp.back_loop_sep" true;
    let s2 = Solver1.solve box xs vs in
    let module S = Set.Make (S.Var) in
    let s = ref S.empty in
    H.iter (fun k v -> s := S.add k !s) s1;
    H.iter (fun k v -> s := S.add k !s) s2;
    S.iter (report s1 s2) !s;
    Printf.printf "\nequal=%d\tsmaller=%d\tbigger=%d\tuncomp=%d\ttotal=%d\n" !equal !smaller !bigger !uncomp (S.cardinal !s);
    s1
end


module WideningSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  module Solver = HelmutBoxSolver (S) (H)
    
  let solve box xs vs = 
    let box1 v x y = 
      S.Dom.join x (S.Dom.widen x (S.Dom.join x y))
    in
    let s = Solver.solve box1 xs vs in
    Printf.printf "|X|=%d\n\n" (H.length s);
    s
end

module HBoxSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
struct
  module Solver = HelmutBoxSolver (S) (H)
    
  let solve box xs vs = 
    let s = Solver.solve box xs vs in
    Printf.printf "|X|=%d\n\n" (H.length s);
    s
end
