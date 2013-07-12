open Batteries
open GobConfig
open Analyses

(** Convert a an [IneqConstrSys] into an equation system by joining all right-hand sides. *)
module SimpleSysConverter (S:IneqConstrSys) 
  : sig include EqConstrSys val conv : S.v -> S.v end 
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
  
  let box = S.box 
  
  let conv x = x
  
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
  
  let box (x,n) = S.box x

  let conv x = (x,-1)
    
  let system (x,n) : ((v -> d) -> (v -> d -> unit) -> d) option = 
    let fold_left1 f xs =
      match xs with
        | [] -> failwith "You promised!!!"
        | x::xs -> List.fold_left f x xs
    in
    match S.system x with
      | []           -> None
      | [f] when n=0 -> Some (fun get set -> f (get % conv) (set % conv))
      | xs when n=(-1) -> 
          let compute get set =
            fold_left1 Dom.join (List.mapi (fun n _ -> get (x,n)) xs)
          in
          Some compute
      | xs -> 
        try Some (fun get set -> List.at xs n (get % conv) (set % conv))
        with Invalid_argument _ -> None 
end 


module SolverStats (S:EqConstrSys) =
struct
  open S
  open Messages
  
  module GU = Goblintutil

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
    Goblintutil.vars := !Goblintutil.vars + 1;
    if tracing
    then trace "sol" "New %a\n" Var.pretty_trace x

  let get_var_event x = 
    if full_trace
    then trace "sol" "Querying %a\n" Var.pretty_trace x
    
    
  let eval_rhs_event x = 
    if full_trace
    then trace "sol" "(Re-)evaluating %a\n" Var.pretty_trace x;
    Goblintutil.evals := !Goblintutil.evals + 1;
    if (get_bool "dbg.solver-progress") then (incr stack_d; print_int !stack_d; flush stdout)
    
  let update_var_event x o n = 
    if tracing then increase x;
    if full_trace || ((not (Dom.is_bot o)) && is_some !max_var && Var.equal (from_some !max_var) x) then begin
      if tracing then tracei "sol" "(%d) Update to %a.\n" !max_c Var.pretty_trace x;
      if tracing then traceu "sol" "%a\n\n" Dom.pretty_diff (n, o)
    end
    
end

(** use this if your [box] is [join] --- the simple solver *)
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

(* use this if you do widenings & narrowings (but no narrowings for globals) --- maybe outdated *)
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



(* use this if you do widenings & narrowings for globals --- outdated *)
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

