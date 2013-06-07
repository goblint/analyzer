
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

module DepSolver (C:CSys) =
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
    let sigma    = VMap.create 1024 (D.bot ()) in
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
      let handle_change (c:V.t) = 
        let u = ref [] in
        let one_infl (y,c) =
          VMap.replace todo y (cons_unique snd c (VMap.find todo y));
          u := y::!u
        in
        List.iter one_infl (VMap.find infl x);
        VMap.remove infl x;
        List.iter one_var !u
      in
      let side_val (x:V.t) (newval:D.t) = 
        (* side-effecting makes it live *)
        if not (VMap.mem sigma x) then one_var x;
        (* increase the value, if needed *)
        let oldval = VMap.find sigma x in
        if not (D.leq newval oldval) then begin
          VMap.replace sigma x (D.join oldval newval);
          (* update dependencies *)
          handle_change x
        end  
      in
      let one_rhs (f,i) = 
        let d = f (eval (x,(f,i))) side_val (evalq (x,(f,i))) in
        newval := D.join !newval d
      in
      if [] = rhss then () else
      List.iter one_rhs rhss;
      side_val x !newval
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
module MC1 : Solver = DepSolver 
module MC2 (C:CSys) : Oracle = DepSolver (C)

module SolverConf (C:CSys) =
struct
  module type S =
  sig
    open C
    val start_val : V.t -> D.t 
    val update_val: V.t -> D.t -> D.t -> D.t
  end
end

module WidenConf (C:CSys) : SolverConf(C).S =
struct
  open C
  let start_val _ = D.bot ()
  let update_val _ = D.widen 
end

module AccuConf (C:CSys) : SolverConf(C).S =
struct
  open C
  let start_val _ = D.bot ()
  let update_val _ = D.join
end

module SimplWConf (C:CSys) : SolverConf(C).S =
struct
  open C
  let start_val _ = D.bot ()
  
  module VM = Hashtbl.Make (V)
  let box_mode       = VM.create 100 (* true -> widen, false -> narrow *)
  let box_statistics = VM.create 100 (* (widen, narrow, switches) *)
  
  let update_val v x y = 
    (*let m = try VM.find box_mode v with Not_found -> ref true in
    let s = try VM.find box_statistics v with Not_found -> ref (0,0,0) in
    let (w,n,sw) = !s in*)
  (*if not (V.loopSep v) then D.join x y
  else*) 
  if D.leq y x then begin 
(*    s := (w,n+1,if !m then sw+1 else sw);
    m := false;*)
    D.narrow x y 
  end else begin (*
    s := (w+1,n,sw);
    m := false;*)
    D.widen x (D.join x y) 
  end
end

let debug = false

module WidenNarrowConf (C:CSys) =
struct
  open C
  let start_val = function 
    | (false, _) -> `Left (D.bot ())
    | (true, _)  -> `Right (D.bot (),D.bot ())
    
  let update_val = function 
     | (false, _) -> fun x y -> begin match x, y with
         | `Left x, `Left y -> `Left (C.D.widen x y)
         | `Left x, `Right (y,z) when C.D.is_bot y && C.D.is_bot z -> `Left x  
         | `Right (y,z), `Left x when C.D.is_bot y && C.D.is_bot z -> `Left x  
         | _ -> failwith "domain broken1"
       end
     | (true, _)  -> fun x y -> begin match x, y with
         | `Right (x1,x2), `Right (y1,y2) ->
           let (t1,t2) = 
             begin
               if D.equal x1 y1 then 
                 (x1, D.meet x2 y2)
               else if D.leq x1 y1 then 
                 (y1, y2)
               else if D.leq y1 x1 then 
                 (x1, x2)
               else (D.join x1 y1, D.top ())
             end in
             if debug then ignore (Pretty.printf "update tuple -- old:\n(%a,%a)\nnew:(%a,%a)" D.pretty x1 D.pretty x2 D.pretty t1 D.pretty t2); 
             `Right (t1,t2)
         | _ -> failwith "domain broken2"
       end
end

module WidenNarrowSys (C:CSys) =
struct 
  open Pretty
  module O = C.O
  module D = 
  struct
    open C
    module ND = Lattice.LexProd (D) (D)
    include Lattice.Either (D) (ND)
  end
  module V = 
  struct
    open C
    type t = bool * V.t
    let context () (_,x)  = V.context () x
    let description (_,x) = V.description x
    let file_name (_,x)   = V.file_name x
    let line_nr (_,x)     = V.line_nr x
    let category (_,x)    = V.category x
    let pretty_trace () (b,x) = (if b then text "N " else text "W ")++V.pretty_trace () x
    let hash = function (true,x)  -> 2*V.hash x
                      | (false,x) -> 2*V.hash x+1
    let equal (b1,x) (b2,y) = b1 = b2 && V.equal x y
    let compare x y =
      match x, y with
        | (true, x), (true, y) -> V.compare x y
        | (false,x), (false,y) -> V.compare x y
        | (true, _), (false,_) -> 1
        | (false,_), (true, _) -> -1
    let loopSep (_,x) = V.loopSep x
  end
  let initvals : (V.t * D.t) list = 
    List.map (fun (x, d) -> (false, x), `Left d) C.initvals 
    
  let getL  = function `Left      x -> x | _ -> failwith "domain brokenQ1"
  let getRL = function `Right (x,_) -> x | _ -> failwith "domain brokenQ2"
  let getRR = function `Right (_,x) -> x | _ -> failwith "domain brokenQ3"

  let constr = function 
    | (false,x) -> 
        let one_rhs f get set ora = 
          let get x = getL (get (false, x)) in
          let set x v = set (false, x) (`Left v) in
          `Left (f get set ora)
        in 
        List.map one_rhs (C.constr x)
    | (true, x) -> 
        let crhs get set ora = 
          let v = getL (get (false, x)) in
          if debug then ignore (printf "transfer for %a:\n%a\n" C.V.pretty_trace x C.D.pretty v); 
          `Right (v,v) 
        in
        let wrhs = C.constr x in
        let nrhs get set ora = 
          try 
            let oldn = get (true, x) in
            let joinwrhs d f =
              let get x = getRR (get (true, x)) in
              let set x v = set (false, x) (`Left v) in (* writing to (false,x) = (`Right (v,v)) is a bug *)
              C.D.join d (f get set ora)
            in 
            let contr = List.fold_left joinwrhs (C.D.bot ()) wrhs in
            if C.D.leq contr (getRR oldn) then begin
              let u = C.D.narrow (getRR oldn) contr in
              if debug then ignore (printf "%a:\n%a narrow %a \n>>>>>>\n%a\n----------\n" 
                  C.V.pretty_trace x C.D.pretty (getRR oldn) C.D.pretty (contr) C.D.pretty u);
              `Right (getRL oldn, u)
            end else `Right (getRL oldn, C.D.join (getRR oldn) contr)
          with Failure d -> failwith ("bla2"^d)
        in
        if List.length wrhs <> 0 then 
          [crhs;nrhs]
        else 
          [crhs]              
end

open Pretty
(* here we assume that side-effecting is done only to variables with an indegree of zero *)
module GenConfSolver (C:CSys) (WN:SolverConf(C).S) =
struct
  open C
  module VMap = Hash.Make (V)
  module QMap = Hash.Make (O.Quest)
  module IMap = BatMap.Make (BatInt)
  type rhs = (V.t -> D.t) -> (V.t -> D.t -> unit) -> (O.Quest.t -> O.answ) -> D.t 
  type t = O.t 
         * (D.t VMap.t) 
         * (D.t IMap.t VMap.t) 
         * ((rhs * int) list VMap.t) 
         * ((V.t * (rhs * int)) list VMap.t) 
         * ((V.t * (rhs * int)) list QMap.t) 
         * ((V.t * (rhs * int)) list ref)
  
  let init sigma = 
    List.iter (fun (v,d) -> VMap.replace sigma v d) initvals
  
  let new_instance () =
    let oracle   = O.new_instance () in
    let sigma    = VMap.create 1024 (D.bot ()) in
    let sigmaw   = VMap.create 1024 IMap.empty in
    let todo     = VMap.create 1024 [] in
    let infl     = VMap.create 1024 [] in
    let inflo    = QMap.create 1024 [] in
    let unsafe   = ref [] in
    init sigma;
    (oracle, sigma, sigmaw, todo, infl, inflo, unsafe)
    
  let change_counter : int VMap.t = VMap.create 1024 (-1)
  let max_n = ref 50 
  let print_diff v d d' = 
    ignore (printf "Variable changed %d times:\n%a\nfrom:\n%a\nto:\n%a\n" !max_n V.pretty_trace v D.pretty d D.pretty d');
    max_n := !max_n * 10
    
  let solve_list (oracle, sigma, sigmaw, todo, infl, inflo, unsafe:t) (xs:V.t list) = 
    let worklist = ref xs in
    let rec one_var (dirty:bool) (x:V.t) = 
      if debug then ignore (printf "considering %a " V.pretty_trace x);
      let rhss =
        if VMap.mem sigma x then begin
          if debug then ignore (printf "(is in sigma)\n");
          let rx = VMap.find todo x in
          VMap.remove todo x;
          rx
        end else begin
          (*VMap.add change_counter x 0;*)
          if debug then ignore (printf "(is NOT in sigma)\n");
          VMap.replace sigma x (WN.start_val x);
          zipNumRev (C.constr x)
        end
      in
      let dirty = ref dirty in
      let handle_change (x:V.t) = 
        let u = ref [] in
        let one_infl (y,c) =
          VMap.replace todo y (cons_unique snd c (VMap.find todo y));
          u := y::!u
        in
        List.iter one_infl (VMap.find infl x);
        VMap.remove infl x;
        List.iter (one_var false) !u
      in 
      let update_con_value x =
        if debug then ignore (printf "updating value for %a" V.pretty_trace x);        
        let oldval = VMap.find sigma x in
        let newval = IMap.fold (fun _ -> D.join) (VMap.find sigmaw x) (D.bot ()) in
        if not (D.equal newval oldval) then begin
          (*VMap.replace change_counter x (VMap.find change_counter x + 1);
          if !max_n <= VMap.find change_counter x then print_diff x oldval newval*)
          (*let newval = WN.update_val x oldval newval in*)
          if debug then ignore (printf " with a new value:\n%a\n" D.pretty newval);        
          VMap.replace sigma x newval;
          handle_change x
        end else begin
          if debug then ignore (printf " but it does not change\n")
        end        
      in
      let update_rhs_value i x d = 
        if debug then ignore (printf "update rhs #%d for %a to:\n%a\n" i V.pretty_trace x D.pretty d);        
        let oldm = try VMap.find sigmaw x with Not_found -> IMap.empty in
        let oldv = try IMap.find i oldm with Not_found -> D.bot () in
        if not (D.equal d oldv) then begin
          let newval = if i = -1 then C.D.join oldv d else WN.update_val x oldv d in
          VMap.replace sigmaw x (IMap.add i newval oldm);
          true
        end else 
          false
      in
      let side_val (x:V.t) (newval:D.t) = 
        if debug then ignore (printf "side-effect ");
        let side_changed = update_rhs_value (-1) x newval in 
        if not (VMap.mem sigma x) then 
          one_var side_changed x 
        else
          update_con_value x
      in
      let one_rhs (f,i) = 
        if debug then ignore (printf "considering rhs %d for %a\n" i V.pretty_trace x);
        let d = f (eval (x,(f,i))) side_val (evalq (x,(f,i))) in
        if debug then ignore (printf "considering rhs %d for %a DONE.\n" i V.pretty_trace x);
        if update_rhs_value i x d then
          dirty := true
      in
      if [] = rhss then () else
      List.iter one_rhs rhss;
      if !dirty then begin
        update_con_value x
      end
    and eval c y =
      if debug then ignore (printf "i need value of %a\n" V.pretty_trace y);
      one_var false y;
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
      List.iter (one_var false) !worklist;
      worklist := [];
      let oneUnsafe (y,c) =
        VMap.replace todo y (cons_unique snd c (VMap.find todo y));
        worklist := y :: !worklist
      in
      List.iter oneUnsafe !unsafe;
      unsafe := []
    done;
    (oracle, sigma, sigmaw, todo, infl, inflo, unsafe)
  
  let solve = solve_list (new_instance ())
  module Quest = V
  type answ  = D.t
  
  let ask (_,sigma,_,_,_,_,_) = VMap.find sigma
  let changed _ = []
end


module GenConfWLSolver (C:CSys) (WN:SolverConf(C).S) =
struct
  open C
  module Q = Queue
  module VMap = Hash.Make (V)
  module QMap = Hash.Make (O.Quest)
  module IMap = BatMap.Make (BatInt)
  type rhs = (V.t -> D.t) -> (V.t -> D.t -> unit) -> (O.Quest.t -> O.answ) -> D.t 
  type t = O.t 
         * (D.t VMap.t) 
         * (D.t IMap.t VMap.t) 
         * ((rhs * int) list VMap.t) 
         * ((V.t * (rhs * int)) list VMap.t) 
         * ((V.t * (rhs * int)) list QMap.t) 
         * ((V.t * (rhs * int)) list ref)
  
  let init sigma = 
    List.iter (fun (v,d) -> VMap.replace sigma v d) initvals
  
  let new_instance () =
    let oracle   = O.new_instance () in
    let sigma    = VMap.create 1024 (D.bot ()) in
    let sigmaw   = VMap.create 1024 IMap.empty in
    let todo     = VMap.create 1024 [] in
    let infl     = VMap.create 1024 [] in
    let inflo    = QMap.create 1024 [] in
    let unsafe   = ref [] in
    init sigma;
    (oracle, sigma, sigmaw, todo, infl, inflo, unsafe)
    
  let solve_list (oracle, sigma, sigmaw, todo, infl, inflo, unsafe:t) (xs:V.t list) = 
    let w = Q.create () in
    let rec one_var (x:V.t) = 
      if debug then ignore (printf "considering %a " V.pretty_trace x);
      let rhss =
        if VMap.mem sigma x then begin
          if debug then ignore (printf "(is in sigma)\n");
          let rx = VMap.find todo x in
          VMap.remove todo x;
          rx
        end else begin
          if debug then ignore (printf "(is NOT in sigma)\n");
          VMap.replace sigma x (WN.start_val x);
          zipNumRev (C.constr x)
        end
      in
      let handle_change (x:V.t) = 
        let one_infl (y,c) =
          VMap.replace todo y (cons_unique snd c (VMap.find todo y));
          Q.add y w
        in
        List.iter one_infl (VMap.find infl x);
      in 
      let update_con_value x =
        if debug then ignore (printf "updating value for %a" V.pretty_trace x);        
        let oldval = VMap.find sigma x in
        let newval = IMap.fold (fun _ -> D.join) (VMap.find sigmaw x) (D.bot ()) in
        if not (D.equal newval oldval) then begin
          (*let newval = WN.update_val x oldval newval in*)
          if debug then ignore (printf " with a new value:\n%a\n" D.pretty newval);        
          VMap.replace sigma x newval;
          handle_change x
        end else 
          if debug then ignore (printf " but it does not change\n");        
      in
      let update_rhs_value i x d = 
        if debug then ignore (printf "update rhs #%d for %a to:\n%a\n" i V.pretty_trace x D.pretty d);        
        let oldm = try VMap.find sigmaw x with Not_found -> IMap.empty in
        let oldv = try IMap.find i oldm with Not_found -> D.bot () in
        if not (D.equal d oldv) then begin
          let newval = if i = -1 then C.D.join oldv d else WN.update_val x oldv d in
          VMap.replace sigmaw x (IMap.add i newval oldm);
          true
        end else 
          false
      in
      let side_val (x:V.t) (newval:D.t) = 
        if debug then ignore (printf "side-effect ");
        let _ = update_rhs_value (-1) x newval in 
        if not (VMap.mem sigma x) then 
          Q.add x w
        else
          update_con_value x
      in
      let one_rhs (f,i) = 
        if debug then ignore (printf "considering rhs %d for %a\n" i V.pretty_trace x);
        let d = f (eval (x,(f,i))) side_val (evalq (x,(f,i))) in
        if debug then ignore (printf "considering rhs %d for %a DONE.\n" i V.pretty_trace x);
        ignore (update_rhs_value i x d) 
      in
      if [] = rhss then () else
      List.iter one_rhs rhss;
      update_con_value x
    and eval c y =
      if debug then ignore (printf "i need value of %a\n" V.pretty_trace y);
      Q.add y w;
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
    
    while not (Q.is_empty w) do
      let wc = Q.copy w in Q.clear w;
      Q.iter one_var wc;
      let oneUnsafe (y,c) =
        VMap.replace todo y (cons_unique snd c (VMap.find todo y));
        Q.add y w
      in
      List.iter oneUnsafe !unsafe;
      unsafe := []
    done;
    (oracle, sigma, sigmaw, todo, infl, inflo, unsafe)
  
  let solve = solve_list (new_instance ())
  module Quest = V
  type answ  = D.t
  
  let ask (_,sigma,_,_,_,_,_) = VMap.find sigma
  let changed _ = []
end

module WNSolver (C:CSys) =
struct  
  module WNC    = WidenNarrowSys (C)
  module WNConf = WidenNarrowConf (C) 
  module Sol = GenConfSolver (WNC) (WNConf)

  module VMap = Sol.VMap
  module QMap = Sol.QMap

  (* ---- *)
  type t = Sol.t
  type answ = C.D.t
  module Quest = C.V
  
  let solve xs = Sol.solve (List.map (fun x -> (true,x)) xs)
  let ask d x = 
    match Sol.ask d (true, x) with
      | `Right (_,x) -> x
      | _ -> failwith "somethings proken"
  let changed x = List.map (fun (_,x) -> x) (List.filter (fun (b,x) -> b) (Sol.changed x))
  let new_instance = Sol.new_instance
end 
module MWC1 : Solver = WNSolver 
module MWC2 (C:CSys) : Oracle = WNSolver (C)


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
        let newval = Val.join oldval v in
        if not (Val.equal newval oldval) then begin
          M.replace m x newval;
          xs := x :: !xs
        end;
        `Ok
end

module SolverTransformer
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Glob.S) =
struct
  open Solver
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
    let module Sol = GenConfSolver (C) (SimplWConf (C))  in
    GU.may_narrow := true;
    let (oh,_), map, _, _, _, _, _ = Sol.solve initialvars in
    let gm = GMap.create (GlobM.length oh) (G.Val.bot ()) in
    let lm = VMap.create (GlobM.length oh) (VDom.bot ()) in
    GlobM.iter    (GMap.add gm) oh;
    (*let f (b,k) = function 
      | `Left d -> ()
      | `Right (s,d) -> 
        (*printf "%a \n>>>>>>\n%a\n----------\n" C.D.pretty s C.D.pretty d;*)
        if b then VMap.add lm k d
    in*)
    Sol.VMap.iter (VMap.add lm) map;
    GU.may_narrow := false;
    lm, gm
    
end

module SolverTransformer2 =
  functor (S:Analyses.GlobConstrSys) ->
  functor (LH:Hash.H with type key=S.LVar.t) ->
  functor (GH:Hash.H with type key=S.GVar.t) ->
struct
  open S
  module GlobM = Hashtbl.Make (GVar)
  module Globs = HashMapOracle (GVar) (G)
  
  let solve : (LVar.t*D.t) list -> (GVar.t*G.t) list -> LVar.t list -> D.t LH.t * G.t GH.t 
            = fun sl sg iv -> 
              
    let module C = 
    struct
      module V = LVar
      module D = D
      module O = Globs
      let initvals = sl
      let constr (x:V.t) = 
        let one_rhs rhs (sigma:V.t -> D.t) (side:V.t -> D.t -> unit) (oracle:O.Quest.t -> O.answ) = 
          let vf x = sigma x in
          let gf x = match oracle (`Get x) with `Val v -> v | _ -> failwith "1" in
          let eff_g x v = ignore (oracle (`Set (x,v))) in
            rhs vf side gf eff_g
        in
        List.map one_rhs (system x)
    end in
    let module Sol = GenConfSolver (C) (SimplWConf (C))  in
    Goblintutil.may_narrow := true;
    let (oh,_), map, _, _, _, _, _ = Sol.solve iv in
    let gm = GH.create (GlobM.length oh) in
    let lm = LH.create (GlobM.length oh) in
    GlobM.iter    (GH.add gm) oh;
    Sol.VMap.iter (LH.add lm) map;
    Goblintutil.may_narrow := false;
    lm, gm
    
end

let _ = 
  Selector.add_solver ("new", (module SolverTransformer2 : Analyses.GenericGlobSolver))

module ClassicalSolver
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Glob.S) =
struct
  open Solver
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
    let module WConf : SolverConf(C).S =
    struct
      open C
      let start_val _ = D.bot ()
      module VM = Hashtbl.Make (V)  
      let update_val v x y = D.widen x (D.join x y)
    end
    in
    let module WSol = GenConfSolver (C) (WConf)  in
    GU.may_narrow := false;
    Printf.printf "Widening phase\n%!";
    let _, map', _, _, _, _, _ = WSol.solve initialvars in
    let module NConf : SolverConf(C).S =
    struct
      open C
      let start_val = WSol.VMap.find map' 
      module VM = Hashtbl.Make (V)  
      let update_val v =  D.narrow
    end
    in
    let module Sol = GenConfSolver (C) (NConf)  in
    GU.may_narrow := true;
    Printf.printf "Narrowing phase\n%!";
    let (oh,_), map, _, _, _, _, _ = Sol.solve initialvars in
    let gm = GMap.create (GlobM.length oh) (G.Val.bot ()) in
    let lm = VMap.create (GlobM.length oh) (VDom.bot ()) in
    GlobM.iter    (GMap.add gm) oh;
    Sol.VMap.iter (VMap.add lm) map;
    GU.may_narrow := false;
    lm, gm
    
end
module Compare
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Glob.S) =
struct
  open Solver
  include Types (Var) (VDom) (G) 
  module S1 = SolverTransformer (Var) (VDom) (G)
  module S2 = ClassicalSolver (Var) (VDom) (G)
  
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' = 
    let f s k e1 =
      let e2 = VMap.find s k in
      match VDom.leq e1 e2, VDom.leq e2 e1 with
        | true , true  -> Printf.printf "="
        | true , false -> Printf.printf "<"
        | false, true  -> Printf.printf ">"
        | false, false -> Printf.printf "?"
    in
    let s1,g1 = S1.solve system initialvars start in
    let s2,_ = S2.solve system initialvars start in
    VMap.iter (f s1) s2;
    s1,g1
end
