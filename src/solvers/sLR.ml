(** The 'slr*' solvers. *)

open Analyses
open Constraints 
open Batteries
open Messages


(** the SLR3 box solver *)
module SLR3 =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct

  let h_find_option h x =
    try Some (HM.find h x)
    with Not_found -> None

  let h_find_default h x d =
    try HM.find h x 
    with Not_found -> d

  let solve box st list =
    (** Helper module for values and priorities. *)
    let module X = 
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
          HM.add vals x (S.Dom.bot ());
          !last_key
                        
      let get_index c = 
        try (HM.find keys c, true) 
        with Not_found -> 
          incr Goblintutil.vars;
          last_key := !last_key - 1; 
          HM.add keys c !last_key;
          HM.add vals c (S.Dom.bot ());
          (!last_key, false)
      
        let to_list () = vals 
    end in
    
    (** Helper module for values of global contributions. *)
    let module XY = 
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
    end in

    (** Helper module for priority queues. *)
    let module H = 
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
        (* ignore @@ Printf.printf "add %d\n" (X.get_key k); *)
        insert h k 
      let extract_min h =
        let (k,h) = extract_min h in
        (* ignore @@ Printf.printf "removing %d\n" (X.get_key k); *)
        (k,h)
    end in
    
    (** Helper module for influence lists. *)
    let module L = 
    struct
      let add h k v = HM.replace h k (v::h_find_default h k [])
      let sub h k = h_find_default h k []
      let rem_item = HM.remove 
    end in
  
    (** Helper module for the stable set and global variable setting deps. *)
    let module P = 
    struct 
      let single x = tap (fun s -> HM.add s x ()) (HM.create 10) 
      let rem_item = HM.remove 
      let to_list s = HM.fold (fun x y z -> x :: z ) s []
      let has_item = HM.mem 
      let rem_item = HM.remove
      let insert m = flip (HM.replace m) ()
    end in
  
    (** Helper module for variable setting deps. *)
    let module T = 
    struct
      let sub = h_find_option 
      let update = HM.replace
      let set    = HM.create 1024 
    end in
        
    (** Helper module for the domain. *)
    let module D =
    struct
       include S.Dom
       let eq = equal
       let cup = join
       let cap = meet
    end in

    let infl   = HM.create 1024 in
    let wpoint = HM.create 1024 in
    let restart_mode = HM.create 1024    in
    let stable = HM.create 1024 in
    let work   = ref H.empty    in
        
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
    
    let eq x get set = 
	    match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> 
            let sides = HM.create 10 in
            let collect_set x v = 
              (* ignore @@ Pretty.printf "set of key: %a:%a \n\n" S.Var.pretty_trace x S.Dom.pretty v; *)
              let _ = X.get_key x in (* set priority immediately! *)
              h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
            in
            let d = f get collect_set in
            HM.iter set sides;
            d
    in 
    
    let restart x =
      let sk = X.get_key x in
      let rec handle_one x =
        let k = X.get_key x in
        let _ = work := H.insert !work x in
        let _ = P.rem_item stable x in
        if k >= sk then () else
          let _ = X.set_value x (D.bot ()) in
          (* ignore @@ Pretty.printf " also restarting %d: %a\n" k S.Var.pretty_trace x; *)
          let w = L.sub infl x in
          let _ = L.rem_item infl x in
          List.iter handle_one w
      in
      (* ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x; *)
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      List.iter handle_one w
    in 
    
    let rec eval x =
      let (xi,_) = X.get_index x in
      fun y ->
        let (i,nonfresh) = X.get_index y in
        let _ = if xi <= i then HM.replace wpoint y () in
        let _ = if (3>2) && xi <= i then work := H.insert (!work) y in
        let _ = if nonfresh then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                    
    and side x y d = 
      let yk, ynonfresh = X.get_index y in
      if X.get_key x > yk then begin
        (* ignore @@ Pretty.printf "wrong order: %d > %d\n\n"  (X.get_key x) yk; *)
        ()
      end;
      
      if (3>1) then HM.replace wpoint y ();
      
      let _ = 
        match T.sub T.set y with 
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      (* ignore @@ Pretty.printf "key: %a -> %a\nold: %a\n\nd: %a\n\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty old S.Dom.pretty d; *)
      let tmp = d in
      (* ignore @@ Pretty.printf "tmp: %a\n\n"  S.Dom.pretty tmp; *)
     
      if not (D.eq tmp old) then begin
        let _ = XY.set_value (x,y) tmp in

        if ynonfresh then
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
        (* ignore @@ Pretty.printf "solving key: %a \n\n" S.Var.pretty_trace x ; *)
        incr Goblintutil.evals;
        let _ = P.insert stable x in
        let old = X.get_value x in

        let tmp = do_side x (eq x (eval x) (side x)) in 
        let use_box = (not (3>1)) || HM.mem wpoint x in
        let restart_mode_x = h_find_default restart_mode x (2*GobConfig.get_int "ana.restart_count") in
        let rstrt = use_box && (3>3) && D.leq tmp old && restart_mode_x <> 0 in
        let tmp = if use_box then box x old tmp else tmp in
        if not (D.eq tmp old) then begin 
          (* ignore @@ Pretty.printf "key: %a \nold: %a\n\nd: %a\n\n" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp; *)
          let _ = X.set_value x tmp in
          if 3>3 && restart_mode_x mod 2 = 1 && not (D.leq tmp old) then 
            HM.replace restart_mode x (restart_mode_x - 1);
          
          if rstrt then begin 
            if restart_mode_x mod 2 = 0 then 
              HM.replace restart_mode x (restart_mode_x - 1);
            restart x 
          end else
            let w = L.sub infl x in
            let w = if use_box then x::w else w in
            let _ = L.rem_item infl x in
            let _ = if (3>2) then HM.remove wpoint x in
            let _ = work := List.fold_left H.insert (!work) w in
                    List.iter (P.rem_item stable) w;
          loop (X.get_key x) 
        end 
      end;
      if (3>2) then HM.remove wpoint x
        
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
        ignore (f (fun x -> one_var x; h_find_default X.vals x (D.bot ())) (fun x _ -> one_var x))
      in
      List.iter one_var xs;
      HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove X.vals x) X.vals
    in
    reachability list;
    
    X.to_list ()

end


module type Version = sig val ver : int end

(** the box solver *)
module Make =
  functor (V:Version) -> 
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
      (* ignore @@ Printf.printf "add %d\n" (X.get_key k); *)
      insert h k 
    let extract_min h =
      let (k,h) = extract_min h in
      (* ignore @@ Printf.printf "removing %d\n" (X.get_key k); *)
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

  let infl   = HM.create 1024 
  let wpoint = HM.create 1024 
  let restart_mode = HM.create 1024 

  let solve box st list =    
    let stable = HM.create 1024 in
    let work   = ref H.empty    in
        
    let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
    let _ = work := H.merge (H.from_list list) !work in 
    
    let eq x get set = 
	    match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> 
            let sides = HM.create 10 in
            let collect_set x v = 
              let _ = X.get_key x in (* set priority immediately! *)
              h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
            in
            let d = f get collect_set in
            HM.iter set sides;
            d
    in 
    
    let restart x =
      let sk = X.get_key x in
      let rec handle_one x =
        let k = X.get_key x in
        let _ = work := H.insert !work x in
        let _ = P.rem_item stable x in
        if k >= sk then () else
          let _ = X.set_value x (D.bot ()) in
          (* ignore @@ Pretty.printf " also restarting %d: %a\n" k S.Var.pretty_trace x; *)
          let w = L.sub infl x in
          let _ = L.rem_item infl x in
          List.iter handle_one w
      in
      (* ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x; *)
      let w = L.sub infl x in
      let _ = L.rem_item infl x in
      List.iter handle_one w
    in 
    
    let rec eval x =
      let (xi,_) = X.get_index x in
      fun y ->
        let (i,nonfresh) = X.get_index y in
        let _ = if xi <= i then HM.replace wpoint y () in
        let _ = if (V.ver>2) && xi <= i then work := H.insert (!work) y in
        let _ = if nonfresh then () else solve y in
        let _ = L.add infl y x in
        X.get_value y
                    
    and side x y d = 
      let yk, ynonfresh = X.get_index y in
      if X.get_key x > yk then begin
        (* ignore @@ Pretty.printf "wrong order: %d > %d\n\n"  (X.get_key x) yk; *)
        ()
      end;
      
      if (V.ver>1) then HM.replace wpoint y ();
      
      let _ = 
        match T.sub T.set y with 
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
      in 

      let old = XY.get_value (x,y) in 
      (* ignore @@ Pretty.printf "key: %a -> %a\nold: %a\n\nd: %a\n\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty old S.Dom.pretty d; *)
      let tmp = d in
      (* ignore @@ Pretty.printf "tmp: %a\n\n"  S.Dom.pretty tmp; *)
     
      if not (D.eq tmp old) then begin
        let _ = XY.set_value (x,y) tmp in

        if ynonfresh then
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
        let use_box = (not (V.ver>1)) || HM.mem wpoint x in
        let restart_mode_x = h_find_default restart_mode x (2*GobConfig.get_int "ana.restart_count") in
        let rstrt = use_box && (V.ver>3) && D.leq tmp old && restart_mode_x <> 0 in
        let tmp = if use_box then box x old tmp else tmp in
        if not (D.eq tmp old) then begin 
          let _ = X.set_value x tmp in
          if V.ver>3 && restart_mode_x mod 2 = 1 && not (D.leq tmp old) then 
            HM.replace restart_mode x (restart_mode_x - 1);
          
          if rstrt then begin 
            if restart_mode_x mod 2 = 0 then 
              HM.replace restart_mode x (restart_mode_x - 1);
            restart x 
          end else
            let w = L.sub infl x in
            let w = if use_box then x::w else w in
            let _ = L.rem_item infl x in
            let _ = if (V.ver>2) then HM.remove wpoint x in
            let _ = work := List.fold_left H.insert (!work) w in
                    List.iter (P.rem_item stable) w;
          loop (X.get_key x) 
        end 
      end;
      if (V.ver>2) then HM.remove wpoint x
        
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
        ignore (f (fun x -> one_var x; h_find_default X.vals x (D.bot ())) (fun x _ -> one_var x))
      in
      List.iter one_var xs;
      HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove X.vals x) X.vals
    in
    reachability list;
    
    X.to_list ()

end


module type MyGenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
sig
  val solve : (S.v -> S.d -> S.d -> S.d) -> (S.v*S.d) list -> S.v list -> S.d H.t
  val wpoint : unit H.t
  val infl :  S.v list H.t
  val h_find_default : 'a H.t -> S.v -> 'a -> 'a
  module X :
  sig
    val keys : int H.t
  end
end

module PrintInfluence =
  functor (Sol:MyGenericEqBoxSolver) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = Sol (S) (HM)
  let solve box x y =
    let ch = Legacy.open_out "test.dot" in
    let r = S1.solve box x y in
    let f k _ =
      let q = if HM.mem S1.wpoint k then " shape=box style=rounded" else "" in
      let s = Pretty.sprint 80 (S.Var.pretty_trace () k) ^ " " ^ string_of_int (try HM.find S1.X.keys k with Not_found -> 0) in
      ignore (Pretty.fprintf ch "%d [label=\"%s\"%s];\n" (S.Var.hash k) (Goblintutil.escape s) q);
      let f y =
        if try HM.find S1.X.keys k > HM.find S1.X.keys y with Not_found -> false then
          ignore (Pretty.fprintf ch "%d -> %d [arrowhead=box style=dashed];\n" (S.Var.hash k) (S.Var.hash y))
        else
          ignore (Pretty.fprintf ch "%d -> %d ;\n" (S.Var.hash k) (S.Var.hash y))
      in 
      List.iter f (try HM.find S1.infl k with Not_found -> [])
    in
    ignore (Pretty.fprintf ch "digraph G {\nedge [arrowhead=vee];\n");
    HM.iter f r;
    ignore (Pretty.fprintf ch "}\n");
    Legacy.close_out_noerr ch;
    r
end

module JoinContr (Sol: GenericEqBoxSolver) =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  include Sol (Generic.SimpleSysConverter (S)) (HM)
end

module MoreVars (Sol: GenericEqBoxSolver) =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module HM1   = Hashtbl.Make (Generic.ExtendInt (S.Var))
  module EqSys = Generic.NormalSysConverter (S) 
  include Sol (EqSys) (HM1)
  let solve box is iv =
    let box' (k,_) x y = box k x y in
    let is' = List.map (fun (k,v) -> EqSys.conv k, v) is in
    let iv' = List.map (fun k -> EqSys.conv k) iv in
    let r = solve box' is' iv' in
    let r' = HM.create (HM1.length r) in
    HM1.iter (fun (k,_) v -> HM.replace r' k (try S.Dom.join v (HM.find r' k) with Not_found -> v)) r;
    r'
end

module TwoPhased =
  functor (V:Version) -> 
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  include Make (V) (S) (HM)
  let solve box is iv = 
    let sd = solve (fun _ x y -> S.Dom.widen x (S.Dom.join x y)) is iv in
    let iv' = HM.fold (fun k _ b -> k::b) sd [] in
    let f v x y = 
      (* ignore (Pretty.printf "changed %a\nold:%a\nnew:%a\n\n" S.Var.pretty_trace v S.Dom.pretty x S.Dom.pretty y); *)
      S.Dom.narrow x y
    in
    solve f [] iv'
end

module JustWiden =
  functor (V:Version) -> 
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  include Make (V) (S) (HM)
  let solve box is iv = 
    solve (fun _ x y -> S.Dom.widen x (S.Dom.join x y)) is iv 
end

let _ =
  let module W1 = GlobSolverFromIneqSolver (JoinContr (JustWiden (struct let ver = 1 end))) in
  let module W2 = GlobSolverFromIneqSolver (JoinContr (JustWiden (struct let ver = 2 end))) in
  let module W3 = GlobSolverFromIneqSolver (JoinContr (JustWiden (struct let ver = 3 end))) in
  Selector.add_solver ("widen1",  (module W1 : GenericGlobSolver));
  Selector.add_solver ("widen2",  (module W2 : GenericGlobSolver));
  Selector.add_solver ("widen3",  (module W3 : GenericGlobSolver));
  let module S2 = GlobSolverFromIneqSolver (JoinContr (TwoPhased (struct let ver = 1 end))) in
  Selector.add_solver ("two",  (module S2 : GenericGlobSolver));
  let module S1 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 1 end))) in
  Selector.add_solver ("new",  (module S1 : GenericGlobSolver));
  Selector.add_solver ("slr+", (module S1 : GenericGlobSolver))
  
let _ =
  let module S1 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 1 end))) in
  let module S2 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 2 end))) in
  let module S3 = GlobSolverFromIneqSolver (JoinContr (SLR3)) in
  let module S4 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 4 end))) in
  Selector.add_solver ("slr1", (module S1 : GenericGlobSolver));
  Selector.add_solver ("slr2", (module S2 : GenericGlobSolver));
  Selector.add_solver ("slr3", (module S3 : GenericGlobSolver));
  Selector.add_solver ("slr4", (module S4 : GenericGlobSolver));
  let module S1p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 1 end)))) in
  let module S2p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 2 end)))) in
  let module S3p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 3 end)))) in
  let module S4p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 4 end)))) in
  Selector.add_solver ("slr1p", (module S1p : GenericGlobSolver));
  Selector.add_solver ("slr2p", (module S2p : GenericGlobSolver));
  Selector.add_solver ("slr3p", (module S3p : GenericGlobSolver));
  Selector.add_solver ("slr4p", (module S4p : GenericGlobSolver));
  let module S1 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 1 end))) in
  let module S2 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 2 end))) in
  let module S3 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 3 end))) in
  let module S4 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 4 end))) in
  Selector.add_solver ("slr1x", (module S1 : GenericGlobSolver));
  Selector.add_solver ("slr2x", (module S2 : GenericGlobSolver));
  Selector.add_solver ("slr3x", (module S3 : GenericGlobSolver));
  Selector.add_solver ("slr4x", (module S4 : GenericGlobSolver));
  let module S1p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 1 end)))) in
  let module S2p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 2 end)))) in
  let module S3p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 3 end)))) in
  let module S4p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 4 end)))) in
  Selector.add_solver ("slr1xp", (module S1p : GenericGlobSolver));
  Selector.add_solver ("slr2xp", (module S2p : GenericGlobSolver));
  Selector.add_solver ("slr3xp", (module S3p : GenericGlobSolver));
  Selector.add_solver ("slr4xp", (module S4p : GenericGlobSolver));
  