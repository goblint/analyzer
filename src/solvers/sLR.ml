(** The 'slr+' and 'restart' solvers. *)

open Analyses
open Constraints 
open Batteries

module type BoolProp  = sig val value : bool end
module      PropTrue  = struct let value = true  end
module      PropFalse = struct let value = false end

(** the box solver *)
module MakeBoxSolver =
  functor (RES:BoolProp) -> 
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
      let compare x y = Pervasives.compare (X.get_key x) (X.get_key y)
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
    let set    = HM.create 1024 in
    let wpoint = HM.create 1024 in
    let work   = ref H.empty    in
        
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
    let _ = List.iter (fun x -> L.add infl x x) list in 
    
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
    
    let just_join x xk = 
      List.for_all (fun x -> X.get_key x >= xk) @@ L.sub infl x
    in
                        
    let box' x xk y z = if just_join x xk then D.join y z else box x y z in
    
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
        (*let _ = L.add infl x x in *)
        List.iter handle_one w 
      in 
      ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x;
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      let _ = L.add infl x x in 
      List.iter handle_one w 
    in 
    
    let rec eval x =
      let (xi,_) = X.get_index x in
      fun y ->
        let (i,nonfresh) = X.get_index y in
        let _ = if i<=xi then HM.replace wpoint x () in
        let _ = if nonfresh then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                    
    and side x y d = 
      let _ = 
        match T.sub set y with 
          | None -> T.update set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      let tmp = box y old d in 
     
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
      match T.sub set x with 
        | None -> a
        | Some p -> 
            let xs = P.to_list p in 
            (*ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
            List.fold_left (fun a z -> D.cup a (XY.get_value (z,x))) a xs

    and solve x = 
      if not (P.has_item stable x) then begin
        let _ = P.insert stable x in
        let old = X.get_value x in
        let tmp = do_side x (eq x (eval x) (side x)) in 
        let rstrt = RES.value && D.leq tmp old in
        let tmp = if true || HM.mem wpoint x then box' x (X.get_key x) old tmp else tmp in
        if D.eq tmp old then 
          loop (X.get_key x)
        else begin 
          let _ = X.set_value x tmp in

          if rstrt then 
            restart x 
          else
            let w = L.sub infl x in
            let _ = L.rem_item infl x in
            let _ = L.add infl x x in
            let h = List.fold_left H.insert (!work) w in
            let _ = work := h in
                    List.iter (P.rem_item stable) w;
                    
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

let _ =
  let module MakeIsGenericEqBoxSolver : GenericEqBoxSolver = MakeBoxSolver (PropFalse) in
  ()

let _ =
  let module M = GlobSolverFromEqSolver(MakeBoxSolver (PropFalse)) in
  Selector.add_solver ("slr+", (module M : GenericGlobSolver));
  let module M1 = GlobSolverFromEqSolver(MakeBoxSolver (PropTrue)) in
  Selector.add_solver ("restart", (module M1 : GenericGlobSolver))
