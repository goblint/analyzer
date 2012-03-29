module GU = Goblintutil
module Types
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Global.S) =
struct
  module VMap = Hash.Make(Var)  
  module GMap = Hash.Make(G.Var)
  type variable    = Var.t
  type global      = G.Var.t
  type var_domain  = VDom.t
  type glob_domain = G.Val.t
  type var_assign  = variable -> var_domain
  type glob_assign = global -> glob_domain
  type glob_diff   = (global * glob_domain) list
  type effect_fun  = [`G of (global * glob_domain) | `L of (variable * var_domain)] -> unit
  type calls       = variable list (* spawned calls from thread creation *)
  type rhs         = var_assign * glob_assign -> effect_fun -> var_domain * calls
  type lhs         = variable
  type constrain   = lhs * rhs  (* constraint is an OCaml keyword *)
  type system      = lhs -> rhs list (* a set of constraints for each variable *)
  type solution    = var_assign * glob_assign
  type solution'   = var_domain VMap.t * glob_domain GMap.t
  
  let garbage_collect (system:system) (vars:variable list) (sigma, theta: solution') : solution' =
    let nsigma = VMap.create (VMap.length sigma) (VDom.bot ()) in
    let ntheta = GMap.create (GMap.length theta) (G.Val.bot ()) in
    let visited = VMap.create 111 false in
    let apply_diff = function
      | `G (var, value) -> GMap.replace ntheta var (GU.joinvalue G.Val.join value (GMap.find ntheta var))
      | `L (var, value) -> VMap.replace nsigma var (GU.joinvalue VDom.join value (VMap.find nsigma var))      
    in
    let rec visit_lhs var =
      if VMap.find visited var then () else begin
        VMap.replace visited var true;
        List.iter (visit_rhs var) (system var)
      end
    and visit_rhs var rhs = 
      let v, c = rhs (get_local, GMap.find ntheta) apply_diff in
      VMap.replace nsigma var (GU.joinvalue VDom.join v (VMap.find nsigma var));
      List.iter visit_lhs c
    and get_local var =
      visit_lhs var;
      VMap.find nsigma var
    in
    List.iter visit_lhs vars;
    (nsigma,ntheta)

  let verify () (system: system) (sigma,theta: solution') =
    Goblintutil.in_verifying_stage := true;
    let correct = ref true in
    let complain_l (v: variable) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Fixpoint not reached at %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" 
                Var.pretty_trace v VDom.pretty lhs VDom.pretty rhs VDom.pretty_diff (rhs,lhs))
    in
    let complain_g (g: global) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Unsatisfied constraint for global %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\n@]" 
                G.Var.pretty_trace g G.Val.pretty lhs G.Val.pretty rhs)
    in
    (* For each variable v which has been assigned value d', would like to check
     * that d' satisfied all constraints. *)
    let verify_var v d' = 
      let verify_constraint rhs =
        let sigma' x = VMap.find sigma x in
        let theta' x = GMap.find theta x in
        (* First check that each (global) delta is included in the (global)
         * invariant. *)
        let check_glob = function
          | `L (l,lv) ->
            let lv' = VMap.find sigma l in 
              if not (VDom.leq lv lv') then 
                complain_l l lv' lv  
          | `G (g,gv) -> 
            let gv' = GMap.find theta g in 
              if not (G.Val.leq gv gv') then 
                complain_g g gv' gv  in
        let (d,s) = rhs (sigma',theta') check_glob in
        (* Then we check that the local state satisfies this constraint. *)
          if not (VDom.leq d d') then
            complain_l v d' d
      in
      let rhs = system v in
        List.iter verify_constraint rhs
    in
      VMap.iter verify_var sigma;
      Goblintutil.in_verifying_stage := false
end

(* SharirPnueli algo *)

type proc = Cil.varinfo (* CIL.something *)

type nodeKind = 
  [ `ProcCall  
  | `ExitOfProc of proc
  | `Other               ]

module type NodeType =
sig
  include Analyses.VarType 
  val kind : t -> nodeKind
end

module Prod (O1:NodeType) (O2:Lattice.S) =
struct
  type t = O1.t * O2.t
  let compare (u1,u2) (v1,v2) =
    match O1.compare u1 v1 with
      | 0 -> O2.compare u2 v2
      | n -> n
  let equal (u1,u2) (v1,v2) = O1.equal u1 v1 && O2.equal u2 v2
  let category (u,_) = O1.category u
  let hash (u,v) = O1.hash u lxor O2.hash v
  let pretty_trace () (u,v) =
    Pretty.dprintf "(%a,%a)" O1.pretty_trace u O2.pretty v
    
  let line_nr (n,_) = O1.line_nr n 
  let file_name (n,_) = O1.file_name n 
  let description (n,_) = O1.description n 
  let context () (_,c) = O1.context () c 
end

(* xxxxxxxxxxxxxxxxxxxxx *)

module type Oracle =
sig
  type t
  module Quest : Hashtbl.HashedType 
  type answ
  val ask          : t -> Quest.t -> answ
  val changed      : t -> Quest.t list
  val new_instance : unit -> t
end

module WiseGuy 
  : Oracle with type t = unit 
            and type Quest.t = unit 
            and type answ = unit =
struct
  type t     = unit

  module Quest = 
  struct
    type t = unit 
    let equal () () = true
    let hash () = 1
  end
  type answ  = unit
  
  let ask () ()       = ()
  let changed ()      = []
  let new_instance () = ()
end

module type CSys =
sig
  module O : Oracle
  module V : Analyses.VarType
  module D : Lattice.S
  
  val initvals : (V.t * D.t) list
  val constr   : V.t -> ((V.t -> D.t) -> (V.t -> D.t -> unit) -> (O.Quest.t -> O.answ) -> D.t) list
end

module type Solver = functor (C:CSys) ->
sig
  type t
  val solve : C.V.t list -> t
    
  module Quest : Hashtbl.HashedType with type t = C.V.t
  type answ  = C.D.t
  
  val ask     : t -> C.V.t -> C.D.t
  val changed : t -> C.V.t list 
end

let cons_unique key x xs =
  let xk = key x in
  if List.exists (fun y -> xk = key y) xs then 
    xs
  else 
    x::xs
    
let zipNumRev xs = fst (List.fold_left (fun (xs,i) x -> (x,i)::xs, i+1) ([],0) xs)

module WLSolver (C:CSys) =
struct
  open C
  module VMap = Hash.Make (V)
  module QMap = Hash.Make (O.Quest)
  type rhs = (V.t -> D.t) -> (V.t -> D.t -> unit) -> (O.Quest.t -> O.answ) -> D.t 
  type t = O.t 
         * (D.t VMap.t) 
         * ((rhs * int) list VMap.t) 
         * ((V.t * (rhs * int)) list VMap.t) 
         * ((V.t * (rhs * int)) list QMap.t) 
         * ((V.t * (rhs * int)) list ref)
  
  let init sigma = 
    List.iter (fun (v,d) -> VMap.replace sigma v d) initvals
  
  let new_instance () =
    let oracle   = O.new_instance () in
    let sigma    = VMap.create 1024 (D.top ()) in
    let todo     = VMap.create 1024 [] in
    let infl     = VMap.create 1024 [] in
    let inflo    = QMap.create 1024 [] in
    let unsafe   = ref [] in
    init sigma;
    (oracle, sigma, todo, infl, inflo, unsafe)
    
  let solve_list (oracle, sigma, todo, infl, inflo, unsafe:t) (xs:V.t list) = 
    let worklist = ref xs in
    let rec one_var (x:V.t) = 
      let rhss, newval =
        if VMap.mem sigma x then begin
          let rx = VMap.find todo x in
          VMap.remove todo x;
          rx, ref (VMap.find sigma x)
        end else begin
          VMap.replace sigma x (D.bot ());
          zipNumRev (C.constr x), ref (D.bot ()) 
        end
      in
      let update_val (x:V.t) (newval:D.t) = 
        begin if not (VMap.mem sigma x) then one_var x end;
        let oldval = VMap.find sigma x in
        if not (D.leq newval oldval) then begin
          VMap.replace sigma x (GU.joinvalue D.join oldval newval);
          let u = ref [] in
          let one_infl (y,c) =
            VMap.replace todo y (cons_unique snd c (VMap.find todo y));
            u := y::!u
          in
          List.iter one_infl (VMap.find infl x);
          VMap.remove infl x;
          List.iter one_var !u
        end  
      in
      let one_rhs (f,i) = 
        let d = f (eval (x,(f,i))) update_val (evalq (x,(f,i))) in
        newval := GU.joinvalue D.join !newval d
      in
      if [] = rhss then () else
      List.iter one_rhs rhss;
      update_val x !newval
    and eval c y =
      one_var y;
      VMap.replace infl y (c :: (VMap.find infl y));
      VMap.find sigma y 
    and evalq c y =
      QMap.replace inflo y (c :: (QMap.find inflo y));
      let r = O.ask oracle y in
      let oracle_change x = 
        let one_infl (y,c) =
          unsafe := (y,c) :: !unsafe;
        in
        List.iter one_infl (QMap.find inflo x);
        QMap.remove inflo x
      in
      List.iter oracle_change (O.changed oracle);
      r
    in 
    
    while [] <> !worklist do
      List.iter one_var !worklist;
      worklist := [];
      let oneUnsafe (y,c) =
        VMap.replace todo y (cons_unique snd c (VMap.find todo y));
        worklist := y :: !worklist
      in
      List.iter oneUnsafe !unsafe;
      unsafe := []
    done;
    (oracle, sigma, todo, infl, inflo, unsafe)
  
  let solve = solve_list (new_instance ())
  module Quest = V
  type answ  = D.t
  
  let ask (_,sigma,_,_,_,_) = VMap.find sigma
  let changed _ = []
end
module MC1 : Solver = WLSolver 
module MC2 (C:CSys) : Oracle = WLSolver (C)

module HashMapOracle (Var:Hashtbl.HashedType) (Val:Lattice.S) 
  : Oracle 
  with type Quest.t = [ `Get of Var.t | `Set of Var.t * Val.t ] 
   and type answ    = [ `Ok | `Val of Val.t ] 
   and type t       = Val.t Hashtbl.Make(Var).t * Var.t list ref =
struct
  module M = Hashtbl.Make (Var)
  type t = Val.t M.t * Var.t list ref
  let new_instance () : t = (M.create 11, ref [])
  module Quest = 
  struct 
    type t = [ `Get of Var.t | `Set of Var.t * Val.t ] 
    let hash = function `Get x -> Var.hash x | `Set (x,v) -> (Var.hash x + 201) * (Val.hash v)
    let equal x y =
      match x, y with
        | `Get x, `Get y -> Var.equal x y
        | `Set (x,v), `Set (y,u) -> Var.equal x y && Val.equal v u
        | _ -> false
  end
  
  type answ = [ `Ok | `Val of Val.t ]
  let changed (_,xs:t) = 
    let r = List.map (fun x -> `Get x) !xs in
    xs := [];
    r
  
  let ask (m,xs) = function 
    | `Get x -> (try `Val (M.find m x) with Not_found -> `Val (Val.bot ()))
    | `Set (x,v) -> 
        let oldval = try M.find m x with Not_found -> Val.bot () in
        let newval = GU.joinvalue Val.join oldval v in
        if not (Val.equal newval oldval) then begin
          M.replace m x newval;
          xs := x :: !xs
        end;
        `Ok
end

module SolverTransformer
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Global.S) =
struct
  include Types (Var) (VDom) (G) 
  module GlobM = Hashtbl.Make (G.Var)
  module Globs = HashMapOracle (G.Var) (G.Val)
  
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' = 
    let module C = 
    struct
      module V = Var
      module D = VDom
      module O = Globs
      let initvals = start
      let constr (x:V.t) = 
        let one_rhs rhs (sigma:V.t -> D.t) (side:V.t -> D.t -> unit) (oracle:O.Quest.t -> O.answ) = 
          let vf x = sigma x in
          let gf x = match oracle (`Get x) with `Val v -> v | _ -> failwith "1" in
          let eff = function 
            | `G (x, v) -> ignore (oracle (`Set (x,v)))
            | `L (x, v) -> side x v
          in
          let constr, calls = rhs (vf, gf) eff in
          List.iter (fun x -> ignore (sigma x)) calls;
          constr
        in
        List.map one_rhs (system x)
    end in
    let module Sol = WLSolver (C) in
    GU.may_narrow := false;
    let (oh,_), map, _, _, _, _ = Sol.solve initialvars in
    let gm = GMap.create (GlobM.length oh) (G.Val.bot ()) in
    let lm = VMap.create (GlobM.length oh) (VDom.bot ()) in
    GlobM.iter    (GMap.add gm) oh;
    Sol.VMap.iter (VMap.add lm) map;
    lm, gm
    
end
  
