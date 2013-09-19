open Analyses
open Constraints 
open Batteries
open Messages

module GU = Goblintutil

exception SolverCannotDoGlobals

module Make3 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  open S
  
  module VS = Set.Make (Var)

  let hm_find_default t x a = try HM.find t x with Not_found -> a

  module P = 
  struct
    type t = S.Var.t * S.Var.t
    let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
    let hash (x1,x2) = (S.Var.hash x1 - 800) * S.Var.hash x2 
  end

  module HPM = Hashtbl.Make (P)

  let hpm_find_default h x d = try HPM.find h x with Not_found -> d

  (** Helper module for values of global contributions. *)
  module XY = 
  struct
    let xy = HPM.create 1024 

    let get_value x = hpm_find_default xy x (S.Dom.bot ())
    let set_value = HPM.replace xy
  end

  let back   = HPM.create 113  (* debug *)
  let infl   = HM.create 113 

  let solve : (v -> d -> d -> d) -> (v*d) list -> v list -> d HM.t = fun box sv iv ->
    let set    = HM.create 113 in
    let sigma  = HM.create 113 in
    let dep    = HM.create 113 in
    let called = ref VS.empty in
    let stable = ref VS.empty in
    let f x old side get set =
      let join_apply (d_in, d_back) rhs =
        let gets  = ref VS.empty in
        let get' y = 
          (if VS.mem y !called then 
            gets := VS.add y !gets
          else
            gets := VS.union !gets (hm_find_default dep y VS.empty));
          let _ = HM.replace dep x (VS.union !gets (hm_find_default dep x VS.empty)) in
          (if VS.mem x !gets then HPM.replace back (x,y) ());
          get y
        in
        let d = rhs get' set in
        if VS.mem x !gets then
          (d_in, Dom.join d_back d)
        else
          (Dom.join d_in d, d_back)
      in
      let _ = HM.replace dep x VS.empty in
      let d_in, d_back = List.fold_left join_apply (Dom.bot (), side) (system x) in
      if Dom.is_bot d_back then d_in else begin
        HM.add infl x (x :: hm_find_default infl x []);
        Dom.join d_in (box x old d_back)
      end
    in
    let rec destabilize x =
      let t = hm_find_default infl x [] in
        HM.replace infl x [];
        List.iter (fun y -> stable := VS.remove y !stable; destabilize y) t
    in
    let rec solve (x : Var.t) =
      if not (VS.mem x !stable || VS.mem x !called) then begin
        if not (HM.mem sigma x) then (HM.add sigma x (Dom.bot ()); HM.add infl x []);
        called := VS.add x !called;
        let rec loop () =
          stable := VS.add x !stable;
          let old    = hm_find_default sigma x (Dom.bot ()) in 
          let newval = f x old (do_side x) (eval x) (side x) in
          if not (Dom.equal old newval) then begin
            HM.replace sigma x newval; destabilize x
          end ; 
          if not (VS.mem x !stable) then loop ()
        in loop ();
        called := VS.remove x !called
      end
      
    and eval x y =
      solve y; 
      HM.replace infl y (x :: hm_find_default infl y []);
      HM.find sigma y
      
    and do_side y = 
      let p = hm_find_default set y VS.empty in
      VS.fold (fun x a -> Dom.join a (XY.get_value (x,y))) p (Dom.bot ())
      
    and side x y d =     
      let _ = 
        HM.replace set y (VS.add x (hm_find_default set y VS.empty))
      in 

      let old = XY.get_value (x,y) in 
      let tmp = box x old d in 
 
      if not (Dom.equal tmp old) then begin
        let _ = XY.set_value (x,y) tmp in
        if not (VS.mem y !stable) then
          solve y
        else
          destabilize y
      end                                               
    in  
      let add_start (v,d) = 
        HM.replace set v (VS.add v (hm_find_default set v VS.empty));
        XY.set_value (v,v) d
      in
      List.iter add_start sv;
      List.iter solve iv;
      sigma
end

module PrintInfluence2 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = Make3 (S) (HM)
  let solve box x y =
    let ch = Legacy.open_out "test.dot" in
    let r = S1.solve box x y in
    let f k _ =
      let s = Pretty.sprint 80 (S.Var.pretty_trace () k) in
      ignore (Pretty.fprintf ch "%d [label=\"%s\"];\n" (S.Var.hash k) (Goblintutil.escape s));
      let f y =
        if S1.HPM.mem S1.back (y,k) then
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

  

module Make2GGS : Analyses.GenericGlobSolver = GlobSolverFromIneqSolver (PrintInfluence2)
let _ =
  Selector.add_solver ("TD", (module Make2GGS : Analyses.GenericGlobSolver))