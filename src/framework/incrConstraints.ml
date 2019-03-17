open Prelude
open Cil
open MyCFG
open Pretty
open Analyses
open GobConfig
open Constraints

type commitID = string

module Node :
sig
  include Hashtbl.HashedType with type t = (node * commitID)
  include Set.OrderedType with type t := (node * commitID)
end =
struct
  type t = node * commitID
  let equal (x,xid) (y,yid) =
    xid = yid &&
    match x,y with
    | Statement s1, Statement s2 -> s1.sid = s2.sid
    | Function f1, Function f2 -> f1.vid = f2.vid
    | FunctionEntry f1, FunctionEntry f2 -> f1.vid = f2.vid
    | _ -> false
  let hash (x,_) =
    match x with
    | Statement s     -> s.sid * 17
    | Function f      -> f.vid
    | FunctionEntry f -> -f.vid

  let compare (x,xid) (y,yid)= match node_compare x y with
    | 0 -> String.compare xid yid
    | n -> n
end


module GlobSolverFromIneqSolver (Sol:GenericIneqBoxSolver)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.LVar.t) ->
    functor (GH:Hash.H with type key=S.GVar.t) ->
    struct
      let lh_find_default h k d = try LH.find h k with Not_found -> d
      let gh_find_default h k d = try GH.find h k with Not_found -> d

      module IneqSys = IneqConstrSysFromGlobConstrSys (S)

      module VH : Hash.H with type key=IneqSys.v = Hashtbl.Make(IneqSys.Var)
      module Sol' = Sol (IneqSys) (VH)

      let getR = function
        | `Left x -> x
        | `Right _ -> S.G.bot ()
        | _ -> undefined ()

      let getL = function
        | `Right x -> x
        | `Left _ -> S.D.top ()
        | _ -> undefined ()

      let solve ls gs l =
        let vs = List.map (fun (x,v) -> `L x, `Right v) ls
                 @ List.map (fun (x,v) -> `G x, `Left  v) gs in
        let sv = List.map (fun x -> `L x) l in
        let hm = Sol'.solve IneqSys.box vs sv in
        let l' = LH.create 113 in
        let g' = GH.create 113 in
        let split_vars = function
          | `L x -> fun y -> LH.replace l' x (S.D.join (getL y) (lh_find_default l' x (S.D.bot ())))
          | `G x -> fun y -> GH.replace g' x (getR y)
        in
        VH.iter split_vars hm;
        (l', g')
    end
module VarFI (LD: Printable.S) (*: sig include Analyses.VarType with type t= (MyCFG.node * commitID) * LD.t end *) =
struct
  type t = (MyCFG.node * commitID) * LD.t

  let category = function
    | ((MyCFG.Statement     s,_),_) -> 1
    | ((MyCFG.Function      f,_),_) -> 2
    | ((MyCFG.FunctionEntry f,_),_) -> 3

  let hashmul x y = if x=0 then y else if y=0 then x else x*y
  let hash x = 1(* 
    match x with
    | ((MyCFG.Statement     s,_),d) -> hashmul (LD.hash d) (s.sid*17)
    | ((MyCFG.Function      f,_), d) -> hashmul (LD.hash d) (f.vid*19)
    | ((MyCFG.FunctionEntry f,_),d) -> hashmul (LD.hash d) (f.vid*23) *)

  let equal ((n1, a),d1) ((n2, a2),d2) = MyCFG.Node.equal n1 n2 && String.equal a a2 && LD.equal d1 d2

  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () x =
    match x with
    | (MyCFG.Statement     s,d) -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
    | (MyCFG.Function      f,d) -> dprintf "call of %s" f.vname
    | (MyCFG.FunctionEntry f,d) -> dprintf "entry state of %s" f.vname

  let pretty_trace () ((n,_), c) = let x = n,c in
    if get_bool "dbg.trace.context" then dprintf "(%a, %a)" pretty x LD.pretty c
    else dprintf "%a on %a \n" pretty x Basetype.ProgLines.pretty (getLocation x)

  let compare ((n1,c1),d1) ((n2, c2),d2) =
    let comp =
      match n1, n2 with
      | MyCFG.FunctionEntry f, MyCFG.FunctionEntry g -> compare f.vid g.vid
      | _                    , MyCFG.FunctionEntry g -> -1
      | MyCFG.FunctionEntry g, _                     -> 1
      | MyCFG.Statement _, MyCFG.Function _  -> -1
      | MyCFG.Function  _, MyCFG.Statement _ -> 1
      | MyCFG.Statement s, MyCFG.Statement l -> compare s.sid l.sid
      | MyCFG.Function  f, MyCFG.Function g  -> compare f.vid g.vid
    in
    if comp == 0 then
    let comp2=
      if !Td3.debug_now then
        print_endline "node eq";
      LD.compare d1 d2 in if comp2 == 0 then String.compare c1 c2 else comp2 else comp

  let printXml f ((n,_),c) =
    Var.printXml f n;
    BatPrintf.fprintf f "<context>\n";
    LD.printXml f c;
    BatPrintf.fprintf f "</context>\n"

  let var_id ((n,_),_) = Var.var_id n

  let line_nr ((n,_),_) = (MyCFG.getLoc n).line
  let file_name ((n,_),_) = (MyCFG.getLoc n).file
  let description (n,_) = sprint 80 (Var.pretty () n)
  let context () (_,c) = LD.pretty () c
  let node ((n,_),_) = n
end

module Compare
    (S:Spec)
    (Sys:GlobConstrSys with module LVar = VarFI (S.C)
                        and module GVar = Basetype.Variables
                        and module D = S.D
                        and module G = S.G)
    (LH:Hash.H with type key=Sys.LVar.t)
    (GH:Hash.H with type key=Sys.GVar.t)
=
struct
  open S

  module PP = Hashtbl.Make (Node)

  let compare_locals h1 h2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f k v1 =
      if not (PP.mem h2 k) then () else
        let v2 = PP.find h2 k in
        let b1 = D.leq v1 v2 in
        let b2 = D.leq v2 v1 in
        if b1 && b2 then
          incr eq
        else if b1 then begin
          if get_bool "solverdiffs" then
            let k = fst k in
            ignore (Pretty.printf "%a @@ %a is more precise using %s:\n%a\n" pretty_node k d_loc (getLoc k) (get_string "solver") D.pretty_diff (v1,v2));
          incr le
        end else if b2 then begin
          if get_bool "solverdiffs" then
            let k = fst k in
            ignore (Pretty.printf "%a @@ %a is more precise using %s:\n%a\n" pretty_node k d_loc (getLoc k) (get_string "comparesolver") D.pretty_diff (v1,v2));
          incr gr
        end else
          incr uk
    in
    PP.iter f h1;
    let k1 = Set.of_enum @@ PP.keys h1 in
    let k2 = Set.of_enum @@ PP.keys h2 in
    let o1 = Set.cardinal @@ Set.diff k1 k2 in
    let o2 = Set.cardinal @@ Set.diff k2 k1 in
    Printf.printf "locals:  eq=%d\t%s=%d[%d]\t%s=%d[%d]\tuk=%d\n" !eq (get_string "solver") !le o1 (get_string "comparesolver") !gr o2 !uk

  let compare_globals g1 g2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      let v2 = try GH.find g2 k with Not_found -> G.bot () in
      let b1 = G.leq v1 v2 in
      let b2 = G.leq v2 v1 in
      if b1 && b2 then
        f_eq ()
      else if b1 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "Global %a is more precise using %s:\n%a\n" Sys.GVar.pretty_trace k (get_string "solver") G.pretty_diff (v1,v2));
        f_le ()
      end else if b2 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "Global %a is more precise using %s:\n%a\n" Sys.GVar.pretty_trace k (get_string "comparesolver") G.pretty_diff (v1,v2));
        f_gr ()
      end else
        f_uk ()
    in
    GH.iter f g1;
    Printf.printf "globals: eq=%d\t%s=%d\t%s=%d\tuk=%d\n" !eq (get_string "solver") !le (get_string "comparesolver") !gr !uk

  let compare_locals_ctx h1 h2 =
    let eq, le, gr, uk, n2 = ref 0, ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      if not (LH.mem h2 k) then incr n2 else
      let v2 = LH.find h2 k in
      let b1 = D.leq v1 v2 in
      let b2 = D.leq v2 v1 in
      if b1 && b2 then
        f_eq ()
      else if b1 then begin
        (* if get_bool "solverdiffs" then *)
        (*   ignore (Pretty.printf "%a @@ %a is more precise using %s:\n%a\n" pretty_node k d_loc (getLoc k) (get_string "solver") D.pretty_diff (v1,v2)); *)
        f_le ()
      end else if b2 then begin
        (* if get_bool "solverdiffs" then *)
        (*   ignore (Pretty.printf "%a @@ %a is more precise using %s:\n%a\n" pretty_node k d_loc (getLoc k) (get_string "comparesolver") D.pretty_diff (v1,v2)); *)
        f_gr ()
      end else
        f_uk ()
    in
    LH.iter f h1;
    (* let k1 = Set.of_enum @@ PP.keys h1 in *)
    (* let k2 = Set.of_enum @@ PP.keys h2 in *)
    (* let o1 = Set.cardinal @@ Set.diff k1 k2 in *)
    (* let o2 = Set.cardinal @@ Set.diff k2 k1 in *)
    Printf.printf "locals_ctx:  eq=%d\t%s=%d\t\t%s=%d\tuk=%d\tn2=%d\n" !eq (get_string "solver") !le (get_string "comparesolver") !gr !uk !n2

  let compare (l1,g1) (l2,g2) =
    let one_ctx (n,_) v h =
      PP.replace h n (try D.join v (PP.find h n) with Not_found -> v);
      h
    in
    let h1 = PP.create 113 in
    let h2 = PP.create 113 in
    let _  = LH.fold one_ctx l1 h1 in
    let _  = LH.fold one_ctx l2 h2 in
    compare_locals h1 h2;
    compare_globals g1 g2;
    compare_locals_ctx l1 l2

end




(** A side-effecting system with globals. *)
module type GlobConstrSys =
sig
  module LVar : VarType
  module GVar : VarType

  module D : Lattice.S
  module G : Lattice.S

  module I : IncrementalData
  val system : (LVar.t) -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t) list
end

module type FunctionMap =
sig
  val map: (string, Cil.global * string) Hashtbl.t
  module I: IncrementalData
end

(** The main point of this file---generating a [GlobConstrSys] from a [Spec]. *)
module FromSpec (S:Spec) (Cfg:CfgBackward) (Fm: FunctionMap)(* Possibly have to remove the signature *)
   : sig
    include GlobConstrSys with module LVar = VarFI (S.C)
                           and module GVar = Basetype.Variables
                           and module D = S.D
                           and module G = S.G
                           (*
    val tf : (MyCFG.node * commitID) * S.C.t -> (Cil.location * MyCFG.edge) list * (MyCFG.node *commitID) -> (((MyCFG.node * commitID) * S.C.t) -> S.D.t) -> ((MyCFG.node * commitID) * S.C.t -> S.D.t -> unit) -> (Cil.varinfo -> G.t) -> (Cil.varinfo -> G.t -> unit) -> D.t

   (* val tf : MyCFG.node * S.C.t -> (Cil.location * MyCFG.edge) list * MyCFG.node -> ((MyCFG.node * commitID * S.C.t) -> S.D.t) -> (MyCFG.node * S.C.t -> S.D.t -> unit) -> (Cil.varinfo -> G.t) -> (Cil.varinfo -> G.t -> unit) -> D.t
    *)*)
   end
=
struct
  module LVar = VarFI (S.C)
  type lv = LVar.t
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  let full_context = get_bool "exp.full-context"

  module I = Fm.I
  let common_ctx var pval (getl:lv -> ld) (sidel: lv -> ld -> unit) getg sideg : (D.t, G.t) ctx * D.t list ref =
    let r = ref [] in
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now watch this ... *)
    let rec ctx =
      { ask     = query
      ; node    = fst var
      ; context = snd var
      ; local   = pval
      ; global  = getg
      ; presub  = []
      ; postsub = []
      ; spawn   = (fun f d -> let c = S.context d in
                    print_endline ("Looking up: " ^ f.vname);
                    let commit = Hashtbl.find Fm.map f.vname in
                    if not full_context then sidel ((FunctionEntry f, snd commit), c) d;
                    ignore (getl ((Function f, snd commit), c)))
      ; split   = (fun (d:D.t) _ _ -> r := d::!r)
      ; sideg   = sideg
      ; assign = (fun ?name _    -> failwith "Cannot \"assign\" in common context.")
      }
    and query x = S.query ctx x in
    (* ... nice, right! *)
    let pval, diff = S.sync ctx in
    let _ = List.iter (uncurry sideg) diff in
    { ctx with local = pval }, r

  let rec bigsqcup = function
    | []    -> D.bot ()
    | [x]   -> x
    | x::xs -> D.join x (bigsqcup xs)

  let tf_loop var getl sidel getg sideg d =
    let ctx, r = common_ctx var d getl sidel getg sideg in
    bigsqcup ((S.intrpt ctx)::!r)

  let tf_assign var lv e getl sidel getg sideg d =
    let ctx, r = common_ctx var d getl sidel getg sideg in
    bigsqcup ((S.assign ctx lv e)::!r)

  let normal_return r fd ctx sideg =
    let spawning_return = S.return ctx r fd in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let toplevel_kernel_return r fd ctx sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.local else S.return ctx r fd in
    let spawning_return = S.return {ctx with local = st} None MyCFG.dummy_func in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let tf_ret var ret fd getl sidel getg sideg d =
    let ctx, r = common_ctx var d getl sidel getg sideg in
    let d =
      if (fd.svar.vid = MyCFG.dummy_func.svar.vid ||
          List.mem fd.svar.vname (List.map Json.string (get_list "mainfun"))) &&
         (get_bool "kernel" || get_string "ana.osek.oil" <> "")
      then toplevel_kernel_return ret fd ctx sideg
      else normal_return ret fd ctx sideg
    in
    bigsqcup (d::!r)

  let tf_entry var fd getl sidel getg sideg d =
    let ctx, r = common_ctx var d getl sidel getg sideg in
    bigsqcup ((S.body ctx fd)::!r)

  let tf_test var e tv getl sidel getg sideg d =
    let ctx, r = common_ctx var d getl sidel getg sideg in
    bigsqcup ((S.branch ctx e tv)::!r)

  let tf_normal_call ctx lv e f args (getl: lv -> ld) (sidel: lv->ld->unit) getg sideg =
    let combine (cd, fd) = S.combine {ctx with local = cd} lv e f args fd in
    let paths = S.enter ctx lv f args in
    let _     = if not full_context then List.iter (fun (c,v) -> if not (S.D.is_bot v) then let commit = Hashtbl.find Fm.map f.vname in sidel ((FunctionEntry f, snd commit), S.context v) v) paths in
    let paths = List.map (fun (c,v) -> (c, if S.D.is_bot v then v else let commit = Hashtbl.find Fm.map f.vname in getl ((Function f, snd commit), S.context v))) paths in
    let paths = List.filter (fun (c,v) -> not (D.is_bot v)) paths in
    let paths = List.map combine paths in
    List.fold_left D.join (D.bot ()) paths

  let tf_special_call ctx lv f args = S.special ctx lv f args

  let tf_proc var lv e args (getl: lv->ld) (sidel: lv -> ld -> unit) getg sideg d =
    let ctx, r = common_ctx var d getl sidel getg sideg in
    let functions =
      match ctx.ask (Queries.EvalFunvar e) with
      | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls []
      | `Bot -> []
      | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () e)))
    in
    let one_function f =
      let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in
      if has_dec && not (LibraryFunctions.use_special f.vname)
      then tf_normal_call ctx lv e f args getl sidel getg sideg
      else tf_special_call ctx lv f args
    in
    if [] = functions then
      d (* because LevelSliceLifter *)
    else
      let funs = List.map one_function functions in
      bigsqcup (funs @ !r)

  let tf var getl sidel getg sideg edge d =
    begin match edge with
      | Assign (lv,rv) -> tf_assign var lv rv
      | Proc (r,f,ars) -> tf_proc var r f ars
      | Entry f        -> tf_entry var f
      | Ret (r,fd)     -> tf_ret var r fd
      | Test (p,b)     -> tf_test var p b
      | ASM _          -> fun _ _ _ _ d -> ignore (M.warn "ASM statement ignored."); d
      | Skip           -> fun _ _ _ _ d -> d
      | SelfLoop       -> tf_loop var
    end getl sidel getg sideg d

  let node_to_string (n: node) = match n with
  | Function f -> f.vname
  | FunctionEntry f -> f.vname
  | Statement s -> "s.sid" ^ string_of_int s.sid

  let tf var getl sidel getg sideg (_,edge) d (f,t) =
    print_endline @@ "Node: " ^ (node_to_string (fst var));
    let old_node = !Tracing.current_node in
    let old_loc  = !Tracing.current_loc in
    let old_loc2 = !Tracing.next_loc in
    let _       = Tracing.current_node := (Some (fst var)) in
    let location = match old_node with
      | Some old_n ->
      (try Tracing.NodeMap.find !Tracing.location_map (old_n) with e -> f)
      | None -> f in
    let _       = Tracing.current_loc := location in
    let _       = Tracing.next_loc := t in
    let d       = tf var getl sidel getg sideg edge d in
    let _       = Tracing.current_loc := old_loc in
    let _       = Tracing.next_loc := old_loc2 in
    d

  let tf ((v,a),c) (edges, (u, i)) (getl: lv -> ld) sidel getg sideg =
    let pval = getl (u,c) in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (getLoc v,[]) in
    List.fold_left2 (|>) pval (List.map (tf (v,Obj.repr (fun () -> c)) getl sidel getg sideg) edges) locs

  let tf (v,c) (e,(u,(i:commitID))) getl sidel getg sideg =
    let old_node = !current_node in
    let _       = current_node := Some u in
    let d       = try tf (v,c) (e,((u,i),i)) getl sidel getg sideg
      with M.StopTheWorld -> D.bot ()
         | M.Bailure s -> Messages.warn_each s; (getl ((u,i),c))  in
    let _       = current_node := old_node in
    d

  let add_commit (x: ((Deriving.Cil.location * MyCFG.edge) list * MyCFG.node) list) (commit: commitID)= List.map (fun (innerList, node)-> (innerList, (node, commit)) ) x

  let print_node (node: node) =  print_endline @@ Pretty.sprint 100 (MyCFG.pretty_node () node)

  let system ((v,i),c) =
    match v with
    | FunctionEntry _ when full_context ->
      [fun _ _ _ _ -> S.val_of c]
    | _ ->  List.map (tf ((v,i),c)) (add_commit (Cfg.prev v) i)  
  end