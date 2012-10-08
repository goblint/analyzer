open Batteries_uni


(** A side-effecting system. *)
module type GenericMonSystem =
sig
  type v    (** variables *)
  type d    (** values    *)
  type 'a m (** basically a monad carrier *)
  
  (** Variables must be hashable, comparable, etc.  *)
  module Var : Analyses.VarType with type t = v
  (** Values must form a lattice. *)
  module Dom : Lattice.S with type t = d
  
  (** the box-operator to be used *)
  val box : v -> d -> d -> d 
  (** The system in functional form. *)
  val system : v -> ((v -> d) -> (v -> d -> unit) -> d) m
end

(** Any system of side-effecting inequations over lattices. *)
module type GenericIneqSystem = GenericMonSystem with type 'a m := 'a list 

(** Any system of side-effecting equations over lattices. *)
module type GenericEqSystem = GenericMonSystem with type 'a m := 'a option 

(** Convert a an [GenericIneqSystem] into an equation system by joining all right-hand sides. *)
module SimpleSysConverter (S:GenericIneqSystem) 
  : GenericEqSystem 
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
  let hash (u,v) = B.hash u * 13 * v
  let pretty_trace () (u,v) =
    Pretty.dprintf "(%a,%n)" B.pretty_trace u  v
    
  let line_nr (n,_) = B.line_nr n 
  let file_name (n,_) = B.file_name n 
  let description (n,_) = B.description n 
  let context () (c,_) = B.context () c 
  let loopSep (n,_) = B.loopSep n
end
  

(** Convert a an [GenericIneqSystem] into an equation system. *)
module NormalSysConverter (S:GenericIneqSystem) 
  : GenericEqSystem 
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
      | [_] -> (x,0)
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

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericLocalBoxSolver =
  functor (S:GenericEqSystem) ->
  functor (H:Hashtbl.S with type key=S.v) ->
sig
  (** The hash-map [solve xs vs] is a local solution for interesting variables [vs],
      reached from starting values [xs].  *)
  val solve : (S.v*S.d) list -> S.v list -> S.d H.t
end

(** use this if your [box] is [join] *)
module DirtyBoxSolver : GenericLocalBoxSolver =
  functor (S:GenericEqSystem) ->
  functor (H:Hashtbl.S with type key = S.v) ->
struct

  let solve xs vs = 
    (* the stabile "set" *)
    let stbl = H.create 1024 in
    (* the influence map *)
    let infl = H.create 1024 in
    (* the solution map *)
    let sol  = H.of_enum (List.enum xs) in
    
    (* solve the variable [x] *)
    let rec solve_one x = 
      (* solve [x] only if it is not stable *)
      if not (H.mem stbl x) then begin
        (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
        if not (H.mem sol x) then H.add sol x (S.Dom.bot ());
        (* mark [x] stable *)
        H.replace stbl x ();
        (* set the new value for [x] *)
        Option.may (fun f -> set x (f (eval x) set)) (S.system x)
      end
      
    (** return the value for [y] and mark its influence on [x] *)
    and eval x y =
      (* solve variable [y] *)
      solve_one y;
      (* add that [x] will be influenced by [y] *)
      H.replace infl y (x :: H.find_default infl y []);
      (* return the value for [y] *)
      H.find sol y
      
    and set x d =
      (* solve variable [y] if it has not been seen before *)
      if not (H.mem sol x) then solve_one x;
      (* do nothing if we have stabilized [x] *)
      let old = H.find sol x in
      if S.Dom.equal old d then begin
        (* set the new value for [x] *)
        H.replace sol x (S.box x old d);
        (* mark dependencies unstable *)
        let deps = H.find_default infl x [] in
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
    solve_all vs; sol
end

(* use this if you do widenings & narrowings (but no narrowings for globals) *)
module SoundBoxSolver : GenericLocalBoxSolver =
  functor (S:GenericEqSystem) ->
  functor (H:Hashtbl.S with type key = S.v) ->
struct

  let solve xs vs = 
    (* the stabile "set" *)
    let stbl = H.create 1024 in
    (* the influence map *)
    let infl = H.create 1024 in
    (* the solution map  *)
    let sol  = H.of_enum (List.enum xs) in
    (* the side-effected solution map  *)
    let sols = H.of_enum (List.enum xs) in
    
    (* solve the variable [x] *)
    let rec solve_one x = 
      (* solve [x] only if it is not stable *)
      if not (H.mem stbl x) then begin
        (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
        if not (H.mem sol x) then H.add sol x (S.Dom.bot ());
        (* mark [x] stable *)
        H.replace stbl x ();
        (* set the new value for [x] *)
        Option.may (fun f -> set x (f (eval x) side)) (S.system x)
      end
      
    (** return the value for [y] and mark its influence on [x] *)
    and eval x y =
      (* solve variable [y] *)
      solve_one y;
      (* add that [x] will be influenced by [y] *)
      H.replace infl y (x :: H.find_default infl y []);
      (* return the value for [y] *)
      H.find sol y
    
    (* this is the function we give to [S.system] *)
    and side x d =
      (* accumulate all side-effects in [sols] *)
      let nd = S.Dom.join d (H.find_default sols x (S.Dom.bot ())) in
      H.replace sols x nd;
      (* do the normal writing operation with the accumulated value *)
      set x nd
      
    and set x d =
      (* solve variable [y] if it has not been seen before *)
      if not (H.mem sol x) then solve_one x;
      (* do nothing if we have stabilized [x] *)
      let old = H.find sol x in
      if S.Dom.equal old d then begin
        (* set the new value for [x] *)
        H.replace sol x (S.box x old d);
        (* mark dependencies unstable *)
        let deps = H.find_default infl x [] in
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
    solve_all vs; sol
end


(* use this if you do widenings & narrowings for globals *)
module PreciseSideEffectBoxSolver : GenericLocalBoxSolver =
  functor (S:GenericEqSystem) ->
  functor (H:Hashtbl.S with type key = S.v) ->
struct

  module VM = Map.Make (S.Var)
  module VS = Set.Make (S.Var)

  let solve xs vs = 
    (* the stabile "set" *)
    let stbl = H.create 1024 in
    (* the influence map *)
    let infl = H.create 1024 in
    (* the solution map  *)
    let sol  = H.create 1024 in
    (* the side-effected solution map  *)
    let sols = H.create 1024 in
    (* the side-effected solution map  *)
    let sdeps = H.create 1024 in
    
    (* solve the variable [x] *)
    let rec solve_one x = 
      (* solve [x] only if it is not stable *)
      if not (H.mem stbl x) then begin
        (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
        if not (H.mem sol x) then H.add sol x (S.Dom.bot ());
        (* mark [x] stable *)
        H.replace stbl x ();
        (* remove side-effected values *)
        H.remove sols x;
        (* set the new value for [x] *)
        Option.may (fun f -> set x (f (eval x) (side x))) (S.system x)
      end
      
    (** return the value for [y] and mark its influence on [x] *)
    and eval x y =
      (* solve variable [y] *)
      solve_one y;
      (* add that [x] will be influenced by [y] *)
      H.replace infl y (x :: H.find_default infl y []);
      (* return the value for [y] *)
      H.find sol y
    
    (* this is the function we give to [S.system] *)
    and side y x d =
      (* mark that [y] has a side-effect to [x] *)
      H.replace sdeps x (VS.add y (H.find_default sdeps x VS.empty));
      (* accumulate all side-effects in [sols] *)
      let find_join_sides z d = 
        try S.Dom.join d (VM.find y (H.find sols z))
        with Not_found -> d
      in
      let nd = VS.fold find_join_sides (H.find_default sdeps x VS.empty) d in
      (* save the value in [sols] *)
      let om = H.find_default sols y VM.empty in
      let nm = VM.modify_def (S.Dom.bot ()) x (S.Dom.join d) om in
      H.replace sols y nm;
      (* do the normal writing operation with the accumulated value *)
      set x nd
      
    and set x d =
      (* solve variable [y] if it has not been seen before *)
      if not (H.mem sol x) then solve_one x;
      (* do nothing if we have stabilized [x] *)
      let old = H.find sol x in
      if S.Dom.equal old d then begin
        (* set the new value for [x] *)
        H.replace sol x (S.box x old d);
        (* mark dependencies unstable *)
        let deps = H.find_default infl x [] in
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
    solve_all vs; sol
end


module JustUseEWC =
struct

end
